CFLAGS = -Wall -fPIC -shared -std=c++11
LDFLAGS = -L./c_src -lcantcoap -lstdc++
ERLFLAGS = -I src/ -o ebin/
all: priv/pdu_drv.so
	erlc $(ERLFLAGS) src/*.erl
priv/pdu_drv.so: c_src/cantcoap.h c_src/libcantcoap.a c_src/pdu.cpp
	gcc $(CFLAGS) c_src/pdu.cpp -o priv/pdu_drv.so $(LDFLAGS)
serial_com.o:
	gcc -c serial_com.c
clean:
	rm -rf *.beam erl_crash.dump
	rm -rf ebin/*.beam ebin/erl.crash.dump
	rm -rf priv/*.so
