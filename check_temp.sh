cd src/
make CXX=g++-8 CC=gcc-8
./mind -l 3 ../minidecaf-tests/testcases/step3/add.c
./mind -l 3 ../test.c
./mind -l 5 ../test.c
cd ../minidecaf-tests