
import numpy
from pandas import DataFrame
from math import cos
from math import sin
from math import radians
from math import asin




def getOverlayOneCubeWithPointAmount(trajList1, trajList2, dtime, distX, distY, angDiffConst, pointAmountRatio=0):
	maxTime = trajList1[9] + dtime
	minTime = trajList1[9] - dtime
	trajList3 = trajList2.loc[(trajList2[9] > minTime) & (trajList2[9] < maxTime),]
	if trajList3.size <= 0:
		# print('trajList3.size <= 0')
		return None
	trajList4 = trajList3.loc[abs(trajList3[9] - trajList1[9]) < dtime,]
	if trajList4.size <= 0:
		# print('trajList4.size <= 0, < dtime')
		return None
	trajList4 = trajList4.loc[abs(trajList4[3] - trajList1[3]) < distX,]
	if trajList4.size <= 0:
		# print('trajList4.size <= 0, < distX')
		return None
	trajList4 = trajList4.loc[abs(trajList4[4] - trajList1[4]) < distY,]
	if trajList4.size <= 0:
		# print('trajList4.size <= 0, < distY')
		return None
	trajList4 = trajList4.loc[angleDiff(trajList4[8], trajList1[8]) < angDiffConst,]
	if trajList4.size <= 0:
		# print('trajList4.size <= 0, < angDiffConst')
		return None
	while pointAmountRatio > 0:
		trajList5 = trajList4.loc[abs(trajList4[0] - trajList1[0]) < (trajList1[0] * (1 - pointAmountRatio)),]
		if trajList5.size > 0:
			trajList4 = trajList5
			break
		pointAmountRatio -= 0.1
	if trajList4.size <= 0:
		return None
	n1 = trajList4.shape[0]
	area1 = DataFrame(numpy.zeros((n1, 3), dtype=int))
	area1[0] = trajList4[5].values	# 其它軌
	area1[1] = trajList4[1].values  # 其它軌第幾cube
	area1[2] = trajList4.index.values  # 其它軌此cube在總名單上第幾
	return area1


def getOverlayOneCube(trajList1, trajList2, dtime, distX, distY, angDiffConst):
	maxTime = trajList1[9] + dtime
	minTime = trajList1[9] - dtime
	trajList3 = trajList2.loc[(trajList2[9] > minTime) & (trajList2[9] < maxTime),]
	trajList4 = trajList3.loc[abs(trajList3[9] - trajList1[9]) < dtime,]
	trajList4 = trajList4.loc[abs(trajList4[3] - trajList1[3]) < distX,]
	trajList4 = trajList4.loc[abs(trajList4[4] - trajList1[4]) < distY,]
	trajList4 = trajList4.loc[angleDiff(trajList4[8], trajList1[8]) < angDiffConst,]
	n1 = trajList4.shape[0]
	area1 = DataFrame(numpy.zeros((n1, 3), dtype=int))
	area1[0] = trajList4[5].values  # 其它軌
	area1[1] = trajList4[1].values  # 其它軌第幾cube
	area1[2] = trajList4.index.values  # 其它軌此cube在總名單上第幾
	return area1


def angleDiff(a, b):
	result = abs(a - b)
	result = result.values
	if len(a) > 1:
		index = result > 180
		result[index] = 360 - result[index]
	else:
		if result > 180:
			result = 360 - result
	return result


def getDistance(x, y):
	Long1, Lat1 = x
	Long2, Lat2 = y
	deltaX = cos(radians(Lat2)) * cos(radians(Long2)) \
		 - cos(radians(Lat1)) * cos(radians(Long1))
	deltaY = cos(radians(Lat2)) * sin(radians(Long2)) \
		 - cos(radians(Lat1)) * sin(radians(Long1))
	deltaZ = sin(radians(Lat2)) - sin(radians(Lat1))
	C = numpy.linalg.norm(numpy.array([deltaX, deltaY, deltaZ]))
	deltaSigma = 2 * asin(C / 2)
	d = round(6371 * 1000 * deltaSigma)
	return d





def getCenterDist(row,targetCube):
		return getDistance((row[3],row[4]),(targetCube[3],targetCube[4]))

def getTimeByMinCenterDist(targetNo,area,trajTrain):
		trajTrainCubesIndex = area[2]
		dfTrajTrain = trajTrain.loc[trajTrainCubesIndex, ]
		result = dfTrajTrain.apply(lambda row: getDistance((row[3],row[4]),(targetNo[3],targetNo[4])), axis=1)
		minIndex = result.idxmin()
		minTrajTrain = trajTrain.loc[minIndex,]
		commonTime = trajTrain.loc[minIndex, 10] - trajTrain.loc[minIndex, 9]
		return (commonTime, minIndex, result[minIndex])
# 2020/06/23

def getFullTime2(cube, trajTrainOri, mappingTrain):
	targetTrainOri = trajTrainOri.loc[(trajTrainOri[5] == cube[5]),]
	indexOri = mappingTrain.loc[(mappingTrain[2] == cube[5]) & (mappingTrain[4] == cube[1]), 3]
	result = targetTrainOri.iloc[indexOri.to_list()[-1],]
	return result['postTime']


def getTestResult4OneTarget(targetTrajNo,
	 trajTest,
	 trajTestOri,
	 mappingTest,
	 trajTrain,
	 trajTrainOri,
	 mappingTrain,
	 epLng, epLat,tauOriginal,maxAngleOriginal):
	targetTraj = trajTest.loc[trajTest[5] == targetTrajNo,].copy()
	targetNew = targetTraj.copy()
	realTime = targetNew.iloc[-1, 10] - targetNew.iloc[0, 9]
	totalTime = 0
	n = targetNew.shape[0]
	commonTime = 0

	tempExpResult = numpy.zeros((n, 4), dtype='int')
	tempExpResult[:, 0] = targetNew.iloc[:, 10] - targetNew.iloc[:, 9]
	countAreaNone = 0
	for i2 in range(n):
		if i2 > 0:
			targetNew.iloc[i2, 9] = targetNew.iloc[i2 - 1, 9] + commonTime
		tau = tauOriginal
		maxAngle = maxAngleOriginal
		area = getOverlayOneCubeWithPointAmount(targetNew.iloc[i2,],
			trajTrain,
			tau,
			epLng,
			epLat,
			maxAngle, 0.5)
		if area is None:
			countAreaNone += 1
			print('ERROR: index=', i2, ' area is None')
			flagGiveUp = False
			epLngEpLatMultiply = 1
			while area is None:
				if tau < 86400:
					tau += 3600
				elif maxAngle < 180:
					maxAngle = 180
				elif epLngEpLatMultiply < 10:
					epLngEpLatMultiply += 1
				else:
					flagGiveUp = True
					break
				area = getOverlayOneCubeWithPointAmount(targetNew.iloc[i2,],
					trajTrain,
					tau,
					epLng * epLngEpLatMultiply,
					epLat * epLngEpLatMultiply,
					maxAngle, 0.5)
			if flagGiveUp:
				print('Give Up!')
				continue

		commonTime, minIndex, dtwResult = getTimeByMinCenterDist(targetNew.iloc[i2, ],
					area,
					trajTrain)
		commonTime2 = getFullTime2(trajTrain.loc[minIndex,], trajTrainOri, mappingTrain)
		commonTime += commonTime2
		tempExpResult[i2, 2] = commonTime
		totalTime += commonTime
		tempExpResult[i2, 3] = dtwResult
		confidence = float(n - countAreaNone) / float(n)
	return (totalTime, realTime, tempExpResult, confidence)
