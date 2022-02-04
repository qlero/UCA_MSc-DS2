# GM-Project
Intrinsic Dimension(ID) estimation via 2 nearest neighbors

## Quick start Windows 10

You need first to install cmake and gnuplot to be able to plot the ID estimator. For more details, please take a look at: [cmake donwload Windows x64 ZIP](https://cmake.org/download/) and [gnuplot download release 5.4.3](http://www.gnuplot.info).

To see a quick result, please just download the repository and build the executable 'main.exe' with the CMakeList.txt as follows:

In the Windows Command Prompt find the folder GM-Project and then follow the next intructions:

```
cmake -S . -B build/ -G "MinGW Makefiles"
cd build
cmake ..
cmake --build . --config Release

```

If everything runs correctly, you should find the executable 'main.exe' in the folder build.

## Test Case

Make sure you are in the folder build and then you can run the following cases with the available data:

Test case 1: Original experiments
```
main.exe -input ../datasets/cauchy20 -discard 0.1
main.exe -input ../datasets/swissroll -discard 0.1
main.exe -input ../datasets/uniform14 -discard 0.1
```
Test case 2: mixtures

```
main.exe -input ../datasets/mix_cauchy20_uniform14 -discard 0.1
main.exe -input ../datasets/mix_uniform7_uniform14 -discard 0.1
```