from __future__ import division
import sys
import gzip
from time import time


def filter_file(fn_in, fn_out, col, match, sep=None):
	gz = fn_in.endswith('.gz')
	f_in = gzip.open(fn_in, 'rb') if gz else open(fn_in, 'rb')
	gz = fn_out.endswith('.gz')
	f_out = gzip.open(fn_out, 'wb') if gz else open(fn_out, 'wb')

	print "filtering lines from %s to %s" % (fn_in, fn_out)
	start = time()
	n, m = 0, 0
	for line in f_in:
		n += 1
		values = line.split(sep)
		if values[col] in match:
			m += 1
			f_out.write(line)
	f_in.close()
	f_out.close()
	ellapsed = time() - start
	print "%d out of %d lines filtered, %.3f%%. time ellapsed: %d" % (
			m, n, m/n*100, ellapsed)

if __name__ == "__main__":
	fn_in = sys.argv[1]
	fn_out = sys.argv[2]
	col = int(sys.argv[3])
	p_flag = sys.argv[4]
	p = sys.argv[5]
	sep = sys.argv[6]
	match = set()
	if p_flag == '-p':
		match.add(p)
	elif p_flag == '-f':
		with open(p, 'rb') as f:
			match = set([line.replace('\n','').replace(sep,'') for line in f])
	else:
		print "error: -p flag not understood %s" % p_flag
		sys.exit()

	filter_file(fn_in, fn_out, col, match, sep)

# 
# 
# 
# 
# 
# 
# 