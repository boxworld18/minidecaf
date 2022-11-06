cd src/
make CXX=g++-8 CC=gcc-8
./mind -l 3 ../test.c
./mind -l 4 ../test.c
./mind -l 5 ../test.c
cd ../minidecaf-tests
./mind -l 3 ../minidecaf-tests/testcases/step9/arg_reg_4.c
