cd src/
make CXX=g++-8 CC=gcc-8
cd ..
cd minidecaf-tests/
STEP_FROM=1 STEP_UNTIL=12 ./check.sh
cd ..