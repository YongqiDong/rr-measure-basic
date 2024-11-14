'''
--- SUPER-ITERATIVE ALGORITHM ---
In this capsule an implementation of the Super-Iterative algorithm is shown [P. Galve et al. (2020)]. Tomographical data are simulated with an image gaussian blurring kernel, linear forward projection of the images and Poisson noise. The low-resolution sinograms are constructed by averaging the full sinogram every three bins in the radial axis. We compare the reconstructions of the high-resolution sinogram, the low-resolution sinogram, and the high-resolution sinograms with interpolations and the super-iterative algorithm. The MLEM algorithm is implemented with gaussian PSF (reduced by a factor 0.8 to avoid Gibbs artifacts and overshoot in the phantom), optional MAP and TV regularizations (set the parameters before running). The results include images and profiles of all the sinograms and the reconstructed images for a similar noise level, and curves of contrast recovery-noise (CRC-noise), CRC-iterations, noise-iterations, and mean squared error-iterations (MSE).
'''

import numpy as np
import numpy.matlib
import matplotlib.pyplot as plt
from skimage.transform import radon, iradon,resize, downscale_local_mean
from skimage.restoration import denoise_tv_chambolle
from scipy import ndimage, signal
from scipy.interpolate import lagrange
from numpy.polynomial.polynomial import Polynomial, polyval
import string


def gen_ph(RES, ACTIVITY):
  # Phantom definition and masks used to measure noise and contrast recovery coefficients (CRC)
  ph = np.asarray([ACTIVITY 
                   if (iy-RES/2+0.5)**2 + (ix-RES/2+0.5)**2 < (RES/2*0.8)**2 #(RES/2*0.8)**2
                   else 0
                   for iy, ix
                   in zip(np.arange(RES*RES)//RES, np.arange(RES*RES)%RES)])
  ph = ph.reshape((RES, RES))
  mask = np.asarray([1 
                   if (iy-RES/2+0.5)**2 + (ix-RES/2+0.5)**2 < (RES/2*0.4)**2 #(RES/2*0.6)**2
                   else 0
                   for iy, ix
                   in zip(np.arange(RES*RES)//RES, np.arange(RES*RES)%RES)])
  mask = mask.reshape((RES, RES))
  masks = []
  masks.append(mask)
  length = 3
  y0 = RES//5 
  nblocks = 0
  while y0 < RES//2-length and nblocks <3: 
    ph[y0 - length: y0 + length + 1, RES//2 - length: RES//2 + length + 1] = ACTIVITY*4 
    aux = np.zeros((RES, RES))
    aux[y0 - length: y0 + length + 1, RES//2 - length: RES//2 + length + 1] = 1
    masks.append(aux)
    y0 += (3*length + 2)
    nblocks += 1
    y0 += length
  masks[0][0: int(RES*0.6), :] = 0
  return ph, masks


def analyze_ph(RES, img, masks):
  # Mesurement of noise and CRC coefficients
  background = masks[0]*img
  background = background.reshape(RES*RES)
  background = background[background > 0]
  back_act = np.mean(background)
  back_std = np.std(background)
  noise = back_std/back_act
  contrast = []
  for i in range(len(masks)-1):
    box_i = masks[i+1]*img
    box_i = box_i[box_i > 0]
    act_i = np.mean(box_i)
    contrast.append((act_i/back_act) / 4)
  return noise, contrast


def interp_Lagrange(Sinog,ran):
  # This function returns a sinogram of shape [RES*3,NANG] using Lagrange interpolations
  # ran is the number of points taken at both sides of the central point to be interpolated
  [RES_low,NANG] = Sinog.shape
  RES = RES_low*3
  Sinog_interp = np.asarray([])
  # We interpolate independently for each angle projection
  for ith in range(NANG): 
      Projection = []
      Projection_copy = Sinog[:,ith] 
      Projection_copy = np.append(np.zeros(ran),Projection_copy)
      Projection_copy = np.append(Projection_copy,np.zeros(ran))
      for ip in range(RES//3): 
          sub_Projection = Projection_copy[ip:ip+2*ran+1] 
          poly = lagrange(range((ip-ran)*3+1,(ip+ran+1)*3+1,3),sub_Projection)
          coefficients = poly.coef[::-1] 
          Proj_interpolation = polyval(np.asarray(range(ip*3,ip*3+3)),coefficients)
          Projection = np.append(Projection,Proj_interpolation)
      Sinog_interp = np.append(Sinog_interp,Projection)
  Sinog_interp = np.asarray([val if val > 0 else 0 for val in Sinog_interp])
  Sinog_interp = Sinog_interp.reshape(NANG,RES).T
  return Sinog_interp


def iradon_mlem(sinog,Niter,x0,sigma,beta_MAP,masks,noise_limit,weight_TV):
    # Function to reconstruct sinogram using Niter iterations of MLEM algorithm
    # beta_MAP is a regularization parameter for the MAP filter (0..1) (0=no regularization)
    # weight_TV is the weight for the total variation filter (weith_TV>0, weight_TV<1e-5 -> no filter)
    # x0 is the reference image (used here for error estimation)
    # sigma is the resolution recovery parameter
    # masks is the variable with the masks for the 
    # noise_limit is the noise level for the output image to be compared
    [RES,NANG] = sinog.shape
    theta = np.linspace(0., 180., NANG, endpoint=False)
    eps = 1e-6 
    x = np.ones([RES,RES])
    # One of every two points is set to zero to reduce the sampling
    norm = np.asarray([1 if (iy-1)%2 == 0  else 0 
                       for iy, ix in zip(np.arange(RES*NANG)//NANG, np.arange(RES*NANG)%NANG)])
    norm = norm.reshape((RES,NANG))
    n = iradon(norm, theta=theta, circle=True,filter=None)
    n = ndimage.gaussian_filter(n,sigma)
    MSE = np.zeros(Niter)
    noise = np.zeros(Niter)
    contrast = np.zeros((len(masks)-1, Niter))
    for iter in range(0,Niter):
      x_b = ndimage.gaussian_filter(x,sigma)
      p = radon(x_b, theta=theta, circle=True)
      ratio = sinog/(p+eps)
      q = iradon(ratio, theta=theta, circle=True, filter=None)
      q = ndimage.gaussian_filter(q,sigma)    
      r = q/(n+eps)
      x = x*r
      x_map = signal.medfilt(x,kernel_size=9)
      x = x/(1.+beta_MAP*(x-x_map)/np.maximum(x_map,eps))
      x = denoise_tv_chambolle(x, weight=weight_TV)

      #Quality metrics
      diffx = (x-x0)*(x-x0)
      MSE[iter] = np.mean(diffx[:])
      noise[iter], contrast[:,iter] = analyze_ph(RES, x, masks)
      if noise[iter]<noise_limit:
        x_out = x

    return (x_out, p, MSE, noise, contrast)


def iradon_mlem_low2high(sinog,Niter,x0,sigma,beta_map,masks,noise_limit,weight_TV):
    # Function to reconstruct low-resolution sinogram using Niter iterations of MLEM algorithm
    # Up-sampling is executed to evaluate noise and CRC
    # beta_MAP is a regularization parameter for the MAP filter (0..1) (0=no regularization)
    # weight_TV is the weight for the total variation filter (weith_TV>0, weight_TV<1e-5 -> no filter)
    # x0 is the reference image (used here for error estimation)
    # sigma is the resolution recovery parameter
    # masks is the variable with the masks for the 
    # noise_limit is the noise level for the output image to be compared
    [RES,NANG] = sinog.shape
    theta = np.linspace(0., 180., NANG, endpoint=False)
    eps = 1e-6 
    x = np.ones([RES,RES])
    # One of every two points is set to zero to reduce the sampling
    norm = np.asarray([1 if (iy-1)%2 == 0  else 0 
                       for iy, ix in zip(np.arange(RES*NANG)//NANG, np.arange(RES*NANG)%NANG)])
    norm = norm.reshape((RES,NANG))
    n = iradon(norm, theta=theta, circle=True,filter=None)
    n = ndimage.gaussian_filter(n,sigma)
    MSE = np.zeros(Niter)
    noise = np.zeros(Niter)
    contrast = np.zeros((len(masks)-1, Niter))
    for iter in range(0,Niter):
      x_b = ndimage.gaussian_filter(x,sigma)
      p = radon(x_b, theta=theta, circle=True)
      ratio = sinog/(p+eps)
      q = iradon(ratio, theta=theta, circle=True, filter=None)
      q = ndimage.gaussian_filter(q,sigma)    
      r = q/(n+eps)
      x = x*r
      x_map = signal.medfilt(x,kernel_size=3)
      x = x/(1.+beta_map*(x-x_map)/np.maximum(x_map,eps))
      x = denoise_tv_chambolle(x, weight=weight_TV)

      #Quality metrics
      x_low2high = resize(x,[RES*3,RES*3], anti_aliasing=True)
      diffx = (x_low2high-x0)*(x_low2high-x0)
      MSE[iter] = np.mean(diffx[:]) 
      noise[iter], contrast[:,iter] = analyze_ph(RES*3, x_low2high, masks)
      if noise[iter]<noise_limit:
        x_out=x

    return (x_out, p, MSE, noise, contrast)


# MAIN FUNCTION - SUPER-ITERATIVE
def superiterative_kernel(RES, NANG, ACTIVITY, sigma, NITER, NSupIter, beta_MAP, weight_TV, noise_limit):
    # RES = Number of pixels in each dimension of the image
    # NANG = Number of angular bins in the sinogram
    # ACTIVITY = Activity in the image (to add Poisson Noise)
    # sigma = blurring in the data and resolution recovery parameter
    # Niter = iterations of MLEM algorithm
    # NSupIter = Number of super-iterations
    # beta_MAP = regularization parameter for the MAP filter (0..1) (0=no regularization)
    # weight_TV = weight for the total variation filter (weith_TV>0, weight_TV<1e-5 -> no filter)
    # noise_limit = equivalent noise level for comparison between images
    
    # DEFINITION OF REFERENCE PHANTOM
    img0, masks = gen_ph(RES, ACTIVITY)
    # We assume gaussian PSF
    img0_blurred = ndimage.gaussian_filter(img0,sigma)

    
    # --- SINOGRAMS ---
    theta = np.linspace(0., 180., NANG, endpoint=False)
    
    # High resolution sinogram
    S0 = radon(img0_blurred, theta=theta, circle=True)
    # One of every two points is set to zero to reduce the sampling
    S0 = np.asarray([S0[iy,ix] if (iy-1)%2 == 0  else 0 for iy, ix
                   in zip(np.arange(RES*NANG)//NANG, np.arange(RES*NANG)%NANG)])
    S0 = S0.reshape((RES,NANG))
    
    # High resolution sinogram + noise 
    S0_poisson = np.random.poisson(S0).astype(float)
    
    # Low resolution sinogram
    # We take just the non-zero elements, and we average over every 3 radial bins
    S_low_poisson = np.asarray([S0_poisson[iy*2+1,ix] for iy, ix
                   in zip(np.arange(RES//2*NANG)//NANG, np.arange(RES//2*NANG)%NANG)])
    S_low_poisson = S_low_poisson.reshape((RES//2,NANG))
    S_low_poisson = downscale_local_mean(S_low_poisson, (3, 1))
    S_low_poisson_low_res = S_low_poisson
    
    # Low resolution sinogram upscaled to high resolution
    S_low_rep = S_low_poisson.repeat(3,axis=0) 
    S_low_rep = np.asarray([S_low_rep[iy//2,ix] if (iy-1)%2 == 0  else 0 for iy, ix
                   in zip(np.arange(RES*NANG)//NANG, np.arange(RES*NANG)%NANG)])
    S_low_rep = S_low_rep.reshape((RES,NANG))
    
    # Low resolution sinogram interpolated to high resolution
    ran=3
    S_interp = interp_Lagrange(S_low_poisson,ran)
    S_interp = np.asarray([S_interp[iy//2,ix] if (iy-1)%2 == 0  else 0 for iy, ix
                   in zip(np.arange(RES*NANG)//NANG, np.arange(RES*NANG)%NANG)])
    S_interp = S_interp.reshape((RES,NANG))
    
    # We recover the null elements in the low resolution sinogram
    S_low_poisson = np.asarray([S_low_poisson[iy//2,ix] if (iy-1)%2 == 0  else 0 for iy, ix
                   in zip(np.arange(RES//3*NANG)//NANG, np.arange(RES//3*NANG)%NANG)])
    S_low_poisson = S_low_poisson.reshape((RES//3,NANG))
    
    
    # --- IMAGE RECONSTRUCTION ---
    # High resolution image
    [X,p,MSE,noise_high,contrast_high] = iradon_mlem(S0_poisson, NITER, img0, 0.8*sigma, beta_MAP, masks, noise_limit, weight_TV)
    # Low resolution image
    [X_low,p2,MSE_low,noise_low,contrast_low] = iradon_mlem_low2high(S_low_poisson/3., NITER, img0, 0.8*sigma/3., beta_MAP, masks, noise_limit, weight_TV)
    # Up-sampled low resolution image
    X_low2high = resize(X_low,[RES,RES], anti_aliasing=True)
    # Low resolution image after sinogram interpolation
    [X_interp,p2,MSE_interp,noise_interp,contrast_interp] = iradon_mlem(S_interp, NITER, img0, 0.8*sigma, beta_MAP, masks, noise_limit, weight_TV)
    
    
    # --- SUPER-ITERATIONS ---
    # Initializing super-iterations...
    XREF = X_low2high
    noise_supiter = []
    contrast_supiter = []
    X_supiter = []
    Y_ik_supiter = []
    MSE_supiter = []
    # Start!
    for isupiter in range(NSupIter):
        # <Y_ik> =  Â*X
        XREF = ndimage.gaussian_filter(XREF, sigma)
        est_Y_ik = radon(XREF, theta=theta, circle=True)
        est_Y_ik = np.asarray([est_Y_ik[iy*2+1,ix] for iy, ix
                      in zip(np.arange(RES//2*NANG)//NANG, np.arange(RES//2*NANG)%NANG)])
        est_Y_ik = est_Y_ik.reshape((RES//2,NANG))
        # Extended sinogram
        Y_i = S_low_poisson_low_res.repeat(3, axis=0)
        # <Y_i> = SUM_k (<Y_ik>)
        sum_Y_ik = downscale_local_mean(est_Y_ik, (3, 1)).repeat(3, axis=0)
        # Y_ik = Y_i*<Y_ik>/<Y_i>   
        Y_ik = Y_i*est_Y_ik / (sum_Y_ik + 1.0e-6) 
        Y_ik = np.asarray([Y_ik[iy//2,ix] if (iy-1)%2 == 0  else 0 for iy, ix
                      in zip(np.arange(RES*NANG)//NANG, np.arange(RES*NANG)%NANG)])
        Y_ik = Y_ik.reshape((RES,NANG)) 
        # Extended Reconstruction
        [XX,pp,MSEE,noise_XXi,contrast_XXi] = iradon_mlem(Y_ik, NITER, img0, 0.8*sigma, beta_MAP, masks, noise_limit, weight_TV)
        XREF = XX
        X_supiter.append(XX)
        Y_ik_supiter.append(Y_ik)
        noise_supiter.append(noise_XXi)
        contrast_supiter.append(contrast_XXi)
        MSE_supiter.append(MSEE)

        
    # --- PLOTTING SINOGRAMS ---
    # Multi-sinogram plot
    fontsize=15
    fig, ax = plt.subplots(3, 2, figsize=(7, 10), dpi=100)
    ax[0,0].set_title("a) Ground Truth", fontsize=fontsize)
    im0 = ax[0,0].imshow(S0[1:RES:2,:], cmap=plt.cm.Greys_r)
    ax[0,0].axis('off')
    ax[0,0].axvline(x=NANG//2, ymin=0, ymax=0.5, c='g', ls='--', lw='2')
    plt.colorbar(im0, ax=ax[0,0])
    ax[0,1].set_title("b) High Resolution", fontsize=fontsize)
    im0 = ax[0,1].imshow(S0_poisson[1:RES:2,:], cmap=plt.cm.Greys_r)
    ax[0,1].axis('off')
    plt.colorbar(im0, ax=ax[0,1])
    ax[1,0].set_title("c) Low Resolution", fontsize=fontsize)
    im0 = ax[1,0].imshow(S_low_rep[1:RES:2,:], cmap=plt.cm.Greys_r)
    ax[1,0].axis('off')
    plt.colorbar(im0, ax=ax[1,0])
    ax[1,1].set_title("d) Interpolated", fontsize=fontsize)
    im0 = ax[1,1].imshow(S_interp[1:RES:2,:], cmap=plt.cm.Greys_r)
    ax[1,1].axis('off')
    plt.colorbar(im0, ax=ax[1,1])
    for isupiter in range(min(NSupIter,2)):
      iplot=string.ascii_lowercase[4+isupiter]
      ax[2,isupiter].set_title(iplot + ") Super-iteration %i" % (isupiter+1), fontsize=fontsize)
      im0 = ax[2,isupiter].imshow(Y_ik_supiter[isupiter][1:RES:2,:], cmap=plt.cm.Greys_r)
      ax[2,isupiter].axis('off')
      plt.colorbar(im0, ax=ax[2,isupiter])
    plt.show()
    fig.tight_layout()
    plt.savefig('../results/Sinograms_multiplot.png')
    
    # Sinogram profile
    plt.figure(figsize=(7, 4.2), dpi=100)
    plt.plot(range(1,RES,2),S0[1:RES:2,NANG//2],'-k', label="Ground Truth")
    plt.plot(range(1,RES,2),S0_poisson[1:RES:2,NANG//2],':+b', label="High Resolution") #b
    plt.plot(range(1,RES,2),S_low_rep[1:RES:2,NANG//2],'--*g', label="Low Resolution") #g
    plt.plot(range(1,RES,2),S_interp[1:RES:2,NANG//2],'-.c.', label="Interpolated") #c
    plt.plot(range(1,RES,2),Y_ik_supiter[NSupIter-1][1:RES:2,NANG//2],linestyle=(0, (3, 1, 1, 1, 1, 1)), color='r', marker='x', label="Super-iteration %i" % (NSupIter)) #r
    plt.grid()
    plt.xlim(RES//2, RES) 
    plt.xlabel("Sinogram profile", fontsize=15)
    plt.ylabel("Activity", fontsize=15)
    plt.legend(loc='upper center', bbox_to_anchor=(.5, 1.2), ncol=3, fontsize=12)
    plt.show()
    plt.tight_layout()
    plt.savefig('../results/Sinogram_profile.png')
    
    
    # --- PLOTTING IMAGES ---
    # Images multiplot
    fig, ax = plt.subplots(3, 2, figsize=(7, 10), dpi=100)
    ax[0,0].set_title("a) Ground Truth", fontsize=fontsize)
    im0 = ax[0,0].imshow(img0, cmap=plt.cm.Greys_r)
    ax[0,0].axis('off')
    ax[0,0].axvline(x=RES//2, ymin=1, ymax=0.5, c='r', ls='--', lw='2')
    plt.colorbar(im0, ax=ax[0,0])
    ax[0,1].set_title("b) High Resolution", fontsize=fontsize)
    im0 = ax[0,1].imshow(X, cmap=plt.cm.Greys_r)
    ax[0,1].axis('off')
    plt.colorbar(im0, ax=ax[0,1])
    ax[1,0].set_title("c) Low Resolution", fontsize=fontsize)
    im0 = ax[1,0].imshow(X_low2high, cmap=plt.cm.Greys_r)
    ax[1,0].axis('off')
    plt.colorbar(im0, ax=ax[1,0])
    ax[1,1].set_title("d) Interpolated", fontsize=fontsize)
    im2 = ax[1,1].imshow(X_interp, cmap=plt.cm.Greys_r)
    ax[1,1].axis('off')
    plt.colorbar(im2, ax=ax[1,1])
    for isupiter in range(min(NSupIter,2)):
      isup_reco=max(NSupIter-2 + isupiter,0)
      iplot=string.ascii_lowercase[4+isupiter]
      ax[2,isupiter].set_title(iplot + ") Super-iteration %i" % (isup_reco+1), fontsize=fontsize)
      im3 = ax[2,isupiter].imshow(X_supiter[isup_reco], cmap=plt.cm.Greys_r)
      ax[2,isupiter].axis('off')
      plt.colorbar(im3, ax=ax[2,isupiter])
    plt.show()
    fig.tight_layout()
    plt.savefig('../results/Images_multiplot.png')
    
    # --- PLOTTING IMAGES WITH ZOOM---
    # Images multiplot
    fig, ax = plt.subplots(3, 2, figsize=(7, 10), dpi=100)
    ax[0,0].set_title("a) Ground Truth", fontsize=fontsize)
    im0 = ax[0,0].imshow(img0[:RES//2,RES//4:RES//4*3], cmap=plt.cm.Greys_r)
    ax[0,0].axis('off')
    ax[0,0].axvline(x=RES//4, ymin=1, ymax=0., c='r', ls='--', lw='2')
    plt.colorbar(im0, ax=ax[0,0])
    ax[0,1].set_title("b) High Resolution", fontsize=fontsize)
    im0 = ax[0,1].imshow(X[:RES//2,RES//4:RES//4*3], cmap=plt.cm.Greys_r)
    ax[0,1].axis('off')
    plt.colorbar(im0, ax=ax[0,1])
    ax[1,0].set_title("c) Low Resolution", fontsize=fontsize)
    im0 = ax[1,0].imshow(X_low2high[:RES//2,RES//4:RES//4*3], cmap=plt.cm.Greys_r)
    ax[1,0].axis('off')
    plt.colorbar(im0, ax=ax[1,0])
    ax[1,1].set_title("d) Interpolated", fontsize=fontsize)
    im2 = ax[1,1].imshow(X_interp[:RES//2,RES//4:RES//4*3], cmap=plt.cm.Greys_r)
    ax[1,1].axis('off')
    plt.colorbar(im2, ax=ax[1,1])
    for isupiter in range(min(NSupIter,2)): 
      isup_reco=max(NSupIter-2 + isupiter,0)
      iplot=string.ascii_lowercase[4+isupiter]
      ax[2,isupiter].set_title(iplot + ") Super-iteration %i" % (isup_reco+1), fontsize=fontsize)
      im3 = ax[2,isupiter].imshow(X_supiter[isup_reco][:RES//2,RES//4:RES//4*3], cmap=plt.cm.Greys_r)
      ax[2,isupiter].axis('off')
      plt.colorbar(im3, ax=ax[2,isupiter])
    plt.show()
    fig.tight_layout()
    plt.savefig('../results/Images_multiplot-zoom.png')

    # Image profile
    plt.figure(figsize=(7, 4.2), dpi=100)
    plt.plot(range(RES),img0[:,RES//2],'-k', label="Ground Truth")
    plt.plot(range(RES),X[:,RES//2],':+b', label="High Resolution") #b
    plt.plot(range(RES),X_low2high[:,RES//2],'--*g', label="Low Resolution") #g
    plt.plot(range(RES),X_interp[:,RES//2],'-.c.', label="Interpolated") #c
    plt.plot(range(RES),X_supiter[NSupIter-1][:,RES//2],linestyle=(0, (3, 1, 1, 1, 1, 1)), color='r', marker='x', label="Super-iteration %i" % (NSupIter)) #r
    plt.grid()
    plt.xlim(0, RES//2) 
    plt.xlabel("Image profile", fontsize=15)
    plt.ylabel("Activity", fontsize=15)
    plt.legend(loc='upper center', bbox_to_anchor=(.5, 1.2), ncol=3, fontsize=12)
    plt.show()
    plt.tight_layout()
    plt.savefig('../results/Image_profile.png')

    
    # --- PLOTTING IMAGE QUALITY PARAMETERS ---
    # CRC-noise
    fig, ax = plt.subplots(3, 1, figsize=(7, 8), dpi=100, sharex=True)
    lw = 2.
    fig.subplots_adjust(hspace=0)
    for iblock in range(min(len(masks)-1,3)):
      ax[iblock].text(0.01, 0.85, 'Box %i' % (iblock+1),
        transform=ax[iblock].transAxes,
        fontsize=16
        )
      ax[iblock].set_ylabel("CRC", fontsize=16)
      ax[iblock].grid()
      ax[iblock].axvline(x=noise_limit, c='k', ls='--', lw='1')
      ax[iblock].plot(noise_high[::10], contrast_high[iblock,::10],':+b', linewidth=lw, label='High Resolution')
      ax[iblock].plot(noise_low[::10], contrast_low[iblock,::10],'--*g', linewidth=lw, label="Low Resolution")
      ax[iblock].plot(noise_interp[::10], contrast_interp[iblock,::10],'-.oc', linewidth=lw, label="Interpolated")
      linestyle=(0, [3,1]) #, 1, 1, 1])
      for i in range(NSupIter):
        label = "Super-iteration %i" % (i+1)
        linestyle[1].append(1)
        linestyle[1].append(1)
        ax[iblock].plot(noise_supiter[i][::10], contrast_supiter[i][iblock,::10], linestyle=linestyle, linewidth=lw, label=label, marker='x')
    leg = ax[iblock].legend(fontsize=14);
    ax[2].set_xlabel("Noise", fontsize=16)
    plt.show()
    fig.tight_layout(h_pad=0)
    plt.savefig('../results/CRC-noise.png')
    
    # CRC-iterations
    fig, ax = plt.subplots(3, 1, figsize=(7, 8), dpi=100, sharex=True)
    lw = 2.
    fig.subplots_adjust(hspace=0)
    for iblock in range(min(len(masks)-1,3)):
      ax[iblock].text(0.01, 0.85, 'Box %i' % (iblock+1),
        transform=ax[iblock].transAxes,
        fontsize=16
        )
      ax[iblock].set_ylabel("CRC", fontsize=16)
      ax[iblock].grid()
      ax[iblock].plot(range(0,NITER,10), contrast_high[iblock,::10],':+b', linewidth=lw, label='High Resolution')
      ax[iblock].plot(range(0,NITER,10), contrast_low[iblock,::10],'--*g', linewidth=lw, label="Low Resolution")
      ax[iblock].plot(range(0,NITER,10), contrast_interp[iblock,::10],'-.oc', linewidth=lw, label="Interpolated")
      linestyle=(0, [3,1]) #, 1, 1, 1])
      for i in range(NSupIter):
        label = "Super-iteration %i" % (i+1)
        linestyle[1].append(1)
        linestyle[1].append(1)
        ax[iblock].plot(range(0,NITER,10), contrast_supiter[i][iblock,::10], linestyle=linestyle, linewidth=lw, label=label, marker='x')
    leg = ax[iblock].legend(fontsize=14);
    ax[2].set_xlabel("Iteration", fontsize=16)
    plt.show()
    fig.tight_layout(h_pad=0)
    plt.savefig('../results/CRC-iter.png')
    
    # Noise-iterations
    fig, ax = plt.subplots(1, 1, figsize=(7, 3), dpi=100, sharex=True)
    lw = 2.
    ax.set_ylabel("Noise", fontsize=16)
    ax.grid()
    ax.axhline(y=noise_limit, c='k', ls='--', lw='1')
    ax.plot(range(0,NITER,10), noise_high[::10],':+b', linewidth=lw, label='High Resolution')
    ax.plot(range(0,NITER,10), noise_low[::10],'--*g', linewidth=lw, label="Low Resolution")
    ax.plot(range(0,NITER,10), noise_interp[::10],'-.oc', linewidth=lw, label="Interpolated")
    linestyle=(0, [3,1]) #, 1, 1, 1])
    for i in range(NSupIter):
      label = "Super-iteration %i" % (i+1)
      linestyle[1].append(1)
      linestyle[1].append(1)
      ax.plot(range(0,NITER,10), noise_supiter[i][::10], linestyle=linestyle, linewidth=lw, label=label, marker='x')
    leg = ax.legend(fontsize=10);
    ax.set_xlabel("Iteration", fontsize=16)
    plt.show()
    fig.tight_layout()
    plt.savefig('../results/noise-iter.png')
    
    # MSE-iterations
    fig, ax = plt.subplots(1, 1, figsize=(7, 3), dpi=100, sharex=True)
    lw = 2.
    ax.set_ylabel("MSE", fontsize=16)
    ax.grid()
    ax.plot(range(0,NITER,10), MSE[::10],':+b', linewidth=lw, label='High Resolution')
    ax.plot(range(0,NITER,10), MSE_low[::10],'--*g', linewidth=lw, label="Low Resolution")
    ax.plot(range(0,NITER,10), MSE_interp[::10],'-.oc', linewidth=lw, label="Interpolated")
    linestyle=(0, [3,1]) #, 1, 1, 1])
    for i in range(NSupIter):
      label = "Super-iteration %i" % (i+1)
      linestyle[1].append(1)
      linestyle[1].append(1)
      ax.plot(range(0,NITER,10), MSE_supiter[i][::10], linestyle=linestyle, linewidth=lw, label=label, marker='x')
    leg = ax.legend(fontsize=14);
    ax.set_yscale('log')
    ax.set_xlabel("Iteration", fontsize=16)
    plt.show()
    fig.tight_layout()
    plt.savefig('../results/MSE-iter.png')

    # MSE-noise
    fig, ax = plt.subplots(1, 1, figsize=(7, 3), dpi=100, sharex=True)
    lw = 2.
    ax.set_ylabel("MSE", fontsize=16)
    ax.grid()
    ax.axvline(x=noise_limit, c='k', ls='--', lw='1')
    ax.plot(noise_high[::10], MSE[::10],':+b', linewidth=lw, label='High Resolution')
    ax.plot(noise_low[::10], MSE_low[::10],'--*g', linewidth=lw, label="Low Resolution")
    ax.plot(noise_interp[::10], MSE_interp[::10],'-.oc', linewidth=lw, label="Interpolated")
    linestyle=(0, [3,1]) #, 1, 1, 1])
    for i in range(NSupIter):
      label = "Super-iteration %i" % (i+1)
      linestyle[1].append(1)
      linestyle[1].append(1)
      ax.plot(noise_supiter[i][::10], MSE_supiter[i][::10], linestyle=linestyle, linewidth=lw, label=label, marker='x')
    leg = ax.legend(fontsize=14);
    ax.set_yscale('log')
    ax.set_xlabel("Noise", fontsize=16)
    plt.show()
    fig.tight_layout()
    plt.savefig('../results/MSE-noise.png')
    
""" 
SELECT PARAMETERS 
*   NRAD = Number of radial bins in the low-resolution sinogram
*   RES = Number of pixels in each dimension of the image (it is equal to 6*NRAD)
*   NANG = Number of angular bins in the sinogram
*   ACTIVITY = Activity in the image (to add Poisson Noise)
*   sigma = System resolution (image blurring before projection)
*   Niter = iterations of MLEM algorithm
*   NSupIter = Number of super-iterations
*   beta_MAP = regularization parameter for the MAP filter (0..1) (0=no regularization)
*   weight_TV = weight for the total variation filter (weith_TV>0, weight_TV<1e-5 -> no filter)
*   noise_limit = equivalent noise level for comparison between images

Recommended values:
*   NRAD >= 32 
*   RES = 6*NRAD
*   NANG >= 90
*   ACTIVITY >= 1.
*   sigma < 3. 
*   NITER > 50 
*   NSupIter < 4
*   beta_MAP < 0.1
*   weight_TV < 0.1
*   noise_limit -> adapt in every case, set a value after convergence in CRC-noise curves is reached
"""

NRAD = 32
RES = NRAD*6 #192 #192 #96 #99 # 93
NANG = 120 #120 #60
ACTIVITY = 1. #10. 
sigma = 2. #1. 
NITER= 200
NSupIter = 2 #2
beta_MAP = 0.005 #0.008 
weight_TV = 0.0000001 
noise_limit = 0.1 #0.1 #0.14 #0.2 #0.05

print('PARAMETERS!')
print('RES=',RES)
print('NANG=',NANG)
print('ACTIVITY=',ACTIVITY)
print('sigma=',sigma)
print('NITER=',NITER)
print('NSupIter=',NSupIter)
print('beta_MAP=',beta_MAP)
print('weight_TV=',weight_TV)
print('noise_limit=',noise_limit)

# SUPER-ITERATIVE METHOD WITH SUBLOR DIVISION
superiterative_kernel(RES, NANG, ACTIVITY, sigma, NITER, NSupIter, beta_MAP, weight_TV, noise_limit)