import shutil
import os

try:
	shutil.rmtree('objects')
except:
	pass

try:
	shutil.rmtree('input-data')
except:
	pass

try:
	shutil.rmtree('logs')
except:
	pass

try:
	os.remove('input-data//train.csv')
except:
	pass

try:
	os.remove('input-data//test.csv')
except:
	pass

try:
	os.remove('input-data//feedback.csv')
except:
	pass