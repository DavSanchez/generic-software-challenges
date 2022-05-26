/**
 * challenge
 *
 * Develop an application so that given two lists of phrases (string of ASCII characters forming words) A and B, finds in which phrase of list B appear all the words contained in each one of the phrases of list A. For example, if all the words of phrase in row 5 of list A appear in phrase located in row 11 of List B, a “A5 B11” output string must be given, and so on for each one of the phrases of list A.
 *
 * The lists A and B will be read from a single ASCII file. Each phrase will be in a different line (that is to say, there is a carriage return at the end of the line). The list A will begin immediately after the line with the marker "LISTA A"; the same for the list B, whose marker is "LISTA B". The first phrase for each list will be considered phrase 0.
 *
 * The comparison will be considered as non case-sensitive. Also, for comparison purposes, it will be considered that acute vocals (áéíóúÁÉÍÓÚ) and non-acute vocals are the same.
 *
 * A “word” will be defined as a contiguous sequence of letters or numbers. Therefore, each word will be delimited (previous and later) by spaces and/or special characters (like punctuation), except the first and last words of each phrase that will be delimited by either: (a) the beginning of a line or (b) EOF or a change of line.
 *
 * The results will be saved to an ASCII output file RESULTS.txt.
 *
 * The data structures for each phrase and the lists of phrases will have to be allocated dynamically and required memory has to increase as needed at run time. A balance between the efficiency of the code (execution speed of the application) and consumption of memory must be considered.
 *
 * For performance purposes, the programmer must assume both lists of phrases are very long.
 **/

#include <iostream>
#include <sstream>
#include <fstream>
#include <set>
#include <vector>

using namespace std;

typedef vector<set<string>> line_vec;

char split_on_nonalpha(char);
pair<line_vec, line_vec> get_lines(ifstream &);
void generate_output_file(pair<line_vec, line_vec>);

//
// Program entry point
//
int main()
{
   ofstream err_log;
   ifstream data("INPUT.txt");
   if (!data.good())
   {
      err_log.open("ERRORS.log");
      err_log << "[ERROR] Could not open file INPUT.txt" << endl;
      err_log << "[ERROR] eof()=" << data.eof() << endl;
      err_log << "[ERROR] fail()=" << data.fail() << endl;
      err_log << "[ERROR] bad()=" << data.bad() << endl;
      err_log << "[INFO] Exiting..." << endl;
      cout << "[ERROR] Check ERRORS.log for details" << endl;
      return 1;
   }
   cout << "[INFO] Reading INPUT.txt" << endl;
   pair<line_vec, line_vec> list_pair = get_lines(data);
   if (list_pair.first.empty() || list_pair.second.empty())
   {
      err_log.open("ERRORS.log");
      err_log << "[ERROR] INPUT.txt is not in the correct format" << endl;
      err_log << "[INFO] Exiting..." << endl;
      cout << "[ERROR] Check ERRORS.log for details" << endl;
      return 1;
   }
   cout << "[INFO] Generating output file RESULTS.txt" << endl;
   generate_output_file(list_pair);
   cout << "[INFO] Done!" << endl;
   return 0;
}

//
// Inserts a space in the place of non-alphanumeric character
//
char split_on_nonalpha(char c)
{
   return isalnum(c) == 0 ? ' ' : c;
}

//
// Generates two vectors of lines, each line corresponding to a set of words.
//
pair<line_vec, line_vec> get_lines(ifstream &data)
{
   line_vec lines_a;
   line_vec lines_b;
   string line;
   while (getline(data, line))
   {
      if (line == "LISTA A")
      {
         line_vec *selected_vec = &lines_a;
         while (getline(data, line))
         {
            if (line == "LISTA B")
            {
               selected_vec = &lines_b; // Begins inserting in the second vector
               continue;
            }
            transform(line.begin(), line.end(), line.begin(), ::tolower);         // Convert to lowercase.
            transform(line.begin(), line.end(), line.begin(), split_on_nonalpha); // Split on non-alpha characters.
            istringstream iss(line);
            string word;
            set<string> phrase;
            while (iss >> word)
            {
               phrase.insert(word);
            }
            selected_vec->push_back(phrase);
         }
      }
   }
   return make_pair(lines_a, lines_b);
}

//
// Prints a report of lines with the format: "A5 B11", meaning that line 5 of list A has all its words contained in line 11 of list B.
//
void generate_output_file(pair<line_vec, line_vec> inputs)
{
   ofstream outfile;
   outfile.open("RESULTS.txt");
   line_vec a = inputs.first;
   line_vec b = inputs.second;
   for (line_vec::iterator it = a.begin(); it != a.end(); ++it)
   {
      set<string> phrase = *it;
      for (line_vec::iterator it2 = b.begin(); it2 != b.end(); ++it2)
      {
         set<string> phrase2 = *it2;
         set<string> intersection;
         set_intersection(phrase.begin(), phrase.end(), phrase2.begin(), phrase2.end(), inserter(intersection, intersection.begin()));
         if (intersection.size() == phrase.size())
         {
            outfile << "A" << distance(a.begin(), it) << " B" << distance(b.begin(), it2) << endl;
         }
      }
   }
   outfile.close();
}
