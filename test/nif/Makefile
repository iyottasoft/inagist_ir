all:
	gcc -fPIC -shared -o ke.so ke.c -I /usr/local/lib/erlang/usr/include/ -I../nif_cpp/ -L../../lib -lnif_test_cpp -lstdc++

clean:
	rm ke.beam ke.so
