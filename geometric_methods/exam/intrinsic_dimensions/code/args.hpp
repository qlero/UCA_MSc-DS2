// Usage: ./main.exe -input <filename> [-discard <fraction>]
// more details in the README file to run the experiments

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cstring>

using namespace std;

// parsing function
// Usage: ./main.exe -input <filename> [-discard <fraction>]
bool parse_command_line(string& filename, double& frac_retained, int argc, char* argv[])
{
	static string usage_string =
		"\n"		
		"Usage: " + string(argv[0]) + " -input <filename> [-discard <fraction>]\n"
		"\n"	
		"       -input:   specify the file from which to read the input data;\n"
		"       -discard: (optional) specify the fraction of points to discard in [0,1) ( default: 0.1)\n"
		"\n";	

	if (argc<=1) {
		cerr << usage_string << flush;
		return false;
	}

	bool found_input = false;
	bool found_format = false;
    bool coordinates = false;
	for (int i=1; i<argc; i++) {
		if (!strcmp(argv[i], "-input")) {
			if(i+1 >= argc) {
				cerr << "\nERROR: Missing argument for option -input\n";
				return false;
			}
			found_input = true;
            found_format = true;
			coordinates = true;
			filename = string(argv[i+1]);
			i++;
		}else if (!strcmp(argv[i], "-discard")) {
			if(i+1 >= argc) {
				cerr << "\nERROR: Missing argument for option -discard\n";
				return false;
			}
			double frac_discard = std::atof(argv[i+1]);
			if (frac_discard < 0. || frac_discard >= 1.) {
				cerr << "\nERROR: the fraction of discarded points must be between 0 and 1 (not included)" << endl;
				return false;
			}
			frac_retained = 1. - frac_discard;
			i++;
		} else {
			cerr << "\nERROR: Unrecognized option " << argv[i] << "\n";
			cerr << usage_string << flush;
			return false;
		}
	}
	
	if (!found_input) {
		cerr << "\nERROR: missing mandatory option -input\n" ;
	}
	if ((!found_input) || (!found_format)) {
		cerr << usage_string << flush;
		return false;
	}
	
	return true;
}
