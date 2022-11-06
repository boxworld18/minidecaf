cd src/
make CXX=g++-8 CC=gcc-8
cd ..
cd minidecaf-tests/
STEP_FROM=9 STEP_UNTIL=9 ./check.sh
cd ..