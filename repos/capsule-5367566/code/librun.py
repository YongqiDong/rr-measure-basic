import pickle
import numpy
from preprocess import trajModel
def run(args,config,lib2,saveModelFilename=None):
	if hasattr(args,'trainDataFile') and getattr(args,'trainDataFile',False):
		oTrainTrajModel = trajModel(config)
		oTrainTrajModel.inputTraj(args.trainDataFile)
	elif hasattr(args,'trajModel') and getattr(args,'trajModel',False):
		with open(args.trajModel, 'rb') as f:
			oTrainTrajModel = pickle.load(f)

	if hasattr(args,'testDataFile') and getattr(args,'testDataFile',False):
		oTestTrajModel = trajModel(config)
		oTestTrajModel.epLat = oTrainTrajModel.epLat
		oTestTrajModel.epLng = oTrainTrajModel.epLng
		oTestTrajModel.epTime = oTrainTrajModel.epTime
		oTestTrajModel.inputTraj(args.testDataFile)
	elif hasattr(args,'testTrajPreprocessed') and getattr(args,'testTrajPreprocessed',False):
		with open(args.testTrajPreprocessed, 'rb') as f:
			oTestTrajModel = pickle.load(f)

	if saveModelFilename:
		with open(saveModelFilename+'_train.pickle', 'wb') as f:
			pickle.dump(oTrainTrajModel, f)
		with open(saveModelFilename+'_test.pickle', 'wb') as f:
			pickle.dump(oTestTrajModel, f)

	trajNoList = numpy.unique(oTestTrajModel.traj[5])
	expResult = numpy.zeros((trajNoList.max() + 1, 6), dtype='float')
	expResult[:, 4] = list(range(trajNoList.max() + 1))
	for targetTrajNo in trajNoList:
		print(targetTrajNo, end=' ', flush=True)
		totalTime, realTime, tempExpResult, confidence = \
			lib2.getTestResult4OneTarget(targetTrajNo,
									oTestTrajModel.traj,
									oTestTrajModel.trajOri,
									oTestTrajModel.mapping,
									oTrainTrajModel.traj,
									oTrainTrajModel.trajOri,
									oTrainTrajModel.mapping,
									oTrainTrajModel.epLng, oTrainTrajModel.epLat, 3600, 10)
		expResult[targetTrajNo, 0] = 1
		expResult[targetTrajNo, 1] = totalTime
		expResult[targetTrajNo, 3] = realTime
		expResult[targetTrajNo, 5] = confidence

	expResult2 = expResult[numpy.where(expResult[:, 0] == 1)[0]]
	deviation = numpy.absolute(expResult2[:, 1] - expResult2[:, 3])
	eachAPE = deviation * 100 / expResult2[:, 3]

	MAPE = eachAPE.mean()
	RMSE = numpy.sqrt((numpy.square(deviation)).mean())
	MAE = deviation.mean()

	if args.DTW:
		outputFilename = '../results/output.txt'
	else:
		outputFilename = '../results/output_noDTW.txt'
	with open(outputFilename, 'w') as f:
		f.write('MAPE=' + str(MAPE) + '\n')
		f.write('RMSE=' + str(RMSE) + '\n')
		f.write('MAE=' + str(MAE) + '\n')

	# if args.testDataFile:
	# 	print('testFileName=', args.testDataFile)
	# elif args.testTrajPreprocessed:
	# 	print('testFileName=', args.testTrajPreprocessed)
	#print('MAPE=', MAPE)
	#print('RMSE=', RMSE)
	#print('MAE=', MAE)
	if args.DTW:
		resultFilename = '../results/expResult.pickle'
	else:
		resultFilename = '../results/expResult_noDTW.pickle'
	with open(resultFilename, 'wb') as f:
		pickle.dump(expResult, f)
	return (MAPE,RMSE,MAE)