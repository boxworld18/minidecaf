cd src/
make CXX=g++-8 CC=gcc-8
cd ..
cd minidecaf-tests/
STEP_FROM=5 STEP_UNTIL=6 ./check.sh
cd ..