all:
	mkdir -p ../../results/latency
	mkdir -p ../../results/streaming
	mkdir -p ../../results/throughput
	python plot-bitrate.py
	python plot-live-latency.py
	python plot-rebuffering.py
	python plot-bitrate-switch-count.py
	python plot-qoe.py
	cd latency && python figure2.py
	cd latency && python figure3.py
	cd throughput && python figure4.py
	cd throughput && python figure9.py
	echo "Done generating figures"