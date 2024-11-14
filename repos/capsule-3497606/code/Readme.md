## README

In this capsule an implementation of the Super-Iterative algorithm is shown [P. Galve et al.]. The method presents a novel strategy to go beyond the expected resolution-noise limits for a given low-resolution (LR) sinogram based on a high-resolution (HR) one. From the projections of the LR reconstructed image, we redistribute the counts of each line-of-response (LOR) into three subLORs to obtain an estimation of the HR sinogram. The whole process of estimation and reconstruction of the HR sinogram is called a super-iteration.

Tomographical data are simulated with an image gaussian blurring kernel, linear forward projection of the images and Poisson noise. One of every two points in the sinogram (radial direction) is set to zero to decrease the sampling. The low-resolution sinograms are constructed by averaging the full sinogram every three bins in the radial axis. We compare the reconstructions of the high-resolution sinogram, the low-resolution sinogram, and the high-resolution sinograms with Lagrange interpolations and the super-iterative algorithm. The MLEM algorithm is implemented with gaussian PSF (reduced by a factor 0.8 to avoid Gibbs artifacts and overshoot in the phantom), optional MAP and TV regularizations (set the parameters before running). The results include images and profiles of all the sinograms and the reconstructed images for a similar noise level, and curves of contrast recovery-noise (CRC-noise), CRC-iterations, noise-iterations, mean squared error-iterations (MSE) and MSE-noise.

The capsule code *Super-Iterative.py* is composed of a main subroutine *superiterative_kernel* and other subroutines to generate and analyze the numerical phantom, and implement the MLEM algorithm. At the end of the code, the main parameters might be selected. 

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

To obtain similar results of those in [P. Galve et al.] use the following parameters:

NRAD =32 \
RES = 192 \
NANG = 120 \
ACTIVITY = 1. \
sigma = 2. \
NITER= 200 \
NSupIter = 2 \
beta_MAP = 0.005 \
weight_TV = 0.0000001 \
noise_limit = 0.10

[P. Galve et al.] P. Galve, J. M. Udias, A. Lopez-Montes, F. Arias, J. J. Vaquero, M. Desco, J. L. Herraiz, "Super-iterative Image Reconstruction in PET", _IEEE Transactions on Computational Imaging_.
