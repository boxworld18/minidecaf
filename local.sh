cd src/
make CXX=g++-8 CC=gcc-8
cd ..
cd minidecaf-tests/
STEP_FROM=11 STEP_UNTIL=11 ./check.sh
cd ..