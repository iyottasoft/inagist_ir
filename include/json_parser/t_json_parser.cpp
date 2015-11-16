/*
 * nix-main.cpp
 * Copyright (C) 2010 Mike Anchor <mikea@mjpa.co.uk>
 *
 * Part of the MJPA JSON Library Demo - http://mjpa.co.uk/blog/view/A-simple-C-JSON-library/
 *
 * License: http://mjpa.co.uk/licenses/GPLv2/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <string>
#include <iostream>
#include <sstream>
#include <iterator>
#include <cstdlib>

#include "JSON.h"

using namespace std;

void example1();
void example2();
void print_out(const char* output);

// Just some sample JSON text, feel free to change but could break demo
const char *EXAMPLE = "\
{ \
  \"string_name\" : \"string\tvalue and a \\\"quote\\\" and a unicode char \\u00BE and a c:\\\\path\\\\ or a \\/unix\\/path\\/ :D\", \
  \"bool_name\" : true, \
  \"bool_second\" : FaLsE, \
  \"null_name\" : nULl, \
  \"negative\" : -34.276, \
  \"sub_object\" : { \
            \"foo\" : \"abc\", \
             \"bar\" : 1.35e2, \
             \"blah\" : { \"a\" : \"A\", \"b\" : \"B\", \"c\" : \"C\" } \
          }, \
  \"array_letters\" : [ \"a\", \"b\", \"c\", [ 1, 2, 3  ]  ] \
}    ";

const char *EXAMPLE3 = "\
{ \
  \"text\" : \"Another example of that politicians more or less suck everywhere\", \
  \"related\" : \"\", \
  \"count\" : 14, \
  \"update_time\" : 0, \
  \"id\" : 20724563147, \
  \"user\" : { \
      \"id\" : 79692602, \
      \"screen_name\" : \"ranvirshorey\", \
      \"profile_image_url\" : \"http:\\/\\/a2.twimg.com\\/profile_images\\/1082818982\\/image_normal.jpg\" \
    }, \
  \"created_at\" : \"Mon Aug 09 17:30:48 +0000 2010\" \
} ";

// Example 1
void example1()
{
  // Parse example data
  JSONValue *value = JSON::Parse(EXAMPLE);
    
  // Did it go wrong?
  if (value == NULL)
  {
    print_out("Example code failed to parse, did you change it?\r\n");
  }
  else
  {
    // Retrieve the main object
    JSONObject root;
    if (value->IsObject() == false)
    {
      print_out("The root element is not an object, did you change the example?\r\n");
    }
    else
    {
      root = value->AsObject();
      
      // Retrieving a string
      if (root.find("string_name") != root.end() && root["string_name"]->IsString())
      {
        print_out("string_name:\r\n");
        print_out("------------\r\n");
        print_out(root["string_name"]->AsString().c_str());
        print_out("\r\n\r\n");
      }
    
      // Retrieving a boolean
      if (root.find("bool_second") != root.end() && root["bool_second"]->IsBool())
      {
        print_out("bool_second:\r\n");
        print_out("------------\r\n");
        print_out(root["bool_second"]->AsBool() ? "it's true!" : "it's false!");
        print_out("\r\n\r\n");
      }
      
      // Retrieving an array
      if (root.find("array_letters") != root.end() && root["array_letters"]->IsArray())
      {
        JSONArray array = root["array_letters"]->AsArray();
        print_out("array_letters:\r\n");
        print_out("--------------\r\n");
        for (unsigned int i = 0; i < array.size(); i++)
        {
          stringstream output;
          output << "[" << i << "] => " << array[i]->Stringify() << "\r\n";
          print_out(output.str().c_str());
        }
        print_out("\r\n");
      }
      
      // Retrieving nested object
      if (root.find("sub_object") != root.end() && root["sub_object"]->IsObject())
      {
        print_out("sub_object:\r\n");
        print_out("-----------\r\n");
        print_out(root["sub_object"]->Stringify().c_str());
        print_out("\r\n\r\n");
      }
    }

    delete value;
  }
}

// Example 2
void example3()
{
  // Parse example data
  JSONValue *value = JSON::Parse(EXAMPLE3);
    
  // Did it go wrong?
  if (value == NULL)
  {
    print_out("Example code failed to parse, did you change it?\r\n");
  }
  else
  {
    // Retrieve the main object
    JSONObject root;
    if (value->IsObject() == false)
    {
      print_out("The root element is not an object, did you change the example?\r\n");
    }
    else
    {
      root = value->AsObject();
      
      // Retrieving a string
      if (root.find("text") != root.end() && root["text"]->IsString())
      {
        print_out("text:\r\n");
        print_out("------------\r\n");
        print_out(root["text"]->AsString().c_str());
        print_out("\r\n\r\n");
      }
      /*
      // Retrieving a boolean
      if (root.find("id") != root.end() && root["id"]->IsLongInt())
      {
        print_out("id:\r\n");
        print_out("------------\r\n");
        print_out(root["id"]->AsBool() ? "it's true!" : "it's false!");
        print_out("\r\n\r\n");
      }
      // Retrieving an array
      if (root.find("array_letters") != root.end() && root["array_letters"]->IsArray())
      {
        JSONArray array = root["array_letters"]->AsArray();
        print_out("array_letters:\r\n");
        print_out("--------------\r\n");
        for (unsigned int i = 0; i < array.size(); i++)
        {
          stringstream output;
          output << "[" << i << "] => " << array[i]->Stringify() << "\r\n";
          print_out(output.str().c_str());
        }
        print_out("\r\n");
      }
      */
      // Retrieving nested object
      if (root.find("user") != root.end() && root["user"]->IsObject())
      {
        print_out("user:\r\n");
        print_out("-----------\r\n");
        print_out(root["user"]->Stringify().c_str());
        print_out("\r\n\r\n");
      }
    }

    delete value;
  }
}

// Example 2
void example2()
{
  JSONObject root;
    
  // Adding a string
  root["test_string"] = new JSONValue("hello world");
    
  // Create a random integer array
  JSONArray array;
  srand((unsigned)time(0));
  for (int i = 0; i < 10; i++)
    array.push_back(new JSONValue((double)(rand() % 100)));
  root["sample_array"] = new JSONValue(array);
    
  // Create a value
  JSONValue *value = new JSONValue(root);
    
  // Print it
  print_out(value->Stringify().c_str());

  // Clean up
  delete value;
}

// Print out function
void print_out(const char *output)
{
  cout << output;
  cout.flush();
}

// Linux entry point
int main(int argc, char **argv)
{
  // Required for utf8 chars
  setlocale(LC_CTYPE, "");
  
  // The mode...
  string mode = argc != 2 ? "" : argv[1];
  
  // Verifying?
  if (mode == "-v" || mode == "-f")
  {
    // Get the stdin data
    cin >> noskipws;
    istream_iterator<char> it(cin);
    istream_iterator<char> end;
    string results(it, end);
    
    // Parse it, print if the test is good/bad
    JSONValue *value = JSON::Parse(results.c_str());
    if ((value != NULL && mode == "-v") || (value == NULL && mode == "-f"))
      cout << "PASS" << endl;
    else
      cout << "FAI" << endl;

    if (value) delete value;
  }
  
  // Parse + echo?
  else if (mode == "-e")
  {
    // Get the stdin data
    cin >> noskipws;
    istream_iterator<char> it(cin);
    istream_iterator<char> end;
    string results(it, end);
    
    // Parse it & echo if it's valid
    JSONValue *value = NULL;
    if ((value = JSON::Parse(results.c_str())) == NULL)
      cout << "Code entered is *NOT* valid.";
    else
      cout << value->Stringify();
    cout << endl;

    if (value) delete value;
  }
  
  // Example 1?
  else if (mode == "-ex1")
  {
    example1();
  }
  
  // Example 2?
  else if (mode == "-ex2")
  {
    example2();
  }

  else if (mode == "-ex3")
  {
    example3();
  }

  // Test cases?
  /*
  else if (mode == "-t")
  {
    run_tests();
  }
  */
  
  // Help!
  else 
  {
    cout << "Usage: " << argv[0] << " <option>" << endl;
    cout << endl;
    cout << "\t-v\tVerify JSON string is *valid* via stdin" << endl;
    cout << "\t-f\tVerify JSON string is *invalid* via stdin" << endl;
    cout << "\t-e\tVerify JSON string via stdin and echo it back using Stringify()" << endl;
    cout << "\t-ex1\tRun example 1 - Example of how to extract data from the JSONValue object" << endl;
    cout << "\t-ex2\tRun example 2 - Building a JSONValue from nothing" << endl;
    cout << "\t-t\tRun test cases" << endl;
    cout << endl;
    cout << "Only one option can be used at a time." << endl;
  }
  
  return 0;
}
