/*
 * JSON.cpp
 * Copyright (C) 2010 Mike Anchor <mikea@mjpa.co.uk>
 *
 * Part of the MJPA JSON Library - http://mjpa.co.uk/blog/view/A-simple-C-JSON-library/
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

#include "JSON.h"
#include <cstring>
#include <cstdlib>

/** 
 * Blocks off the public constructor
 *
 * @access private
 *
 */
JSON::JSON()
{
}

/**
 * Parses a complete JSON encoded string
 * This is just a wrapper around the UNICODE Parse().
 *
 * @access public
 *
 * @param char* data The JSON text
 *
 * @return JSONValue* Returns a JSON Value representing the root, or NULL on error
 */
 /*
JSONValue *JSON::Parse(const char *data)
{
  size_t length = strlen(data) + 1;
  char *w_data = (char*)malloc(length * sizeof(char));
  
  #ifdef WIN32
    size_t ret_value = 0;
    if (mbstowcs_s(&ret_value, w_data, length, data, length) != 0)
    {
      free(w_data);
      return NULL;
    }
  #else
    if (mbstowcs(w_data, data, length) == (size_t)-1)
    {
      free(w_data);
      return NULL;
    }
  #endif
  
  JSONValue *value = JSON::Parse(w_data);
  free(w_data);
  return value;
}
*/

/**
 * Parses a complete JSON encoded string (UNICODE input version)
 *
 * @access public
 *
 * @param char* data The JSON text
 *
 * @return JSONValue* Returns a JSON Value representing the root, or NULL on error
 */
JSONValue *JSON::Parse(const char *data)
{
  // Skip any preceding whitespace, end of data = no JSON = fail
  if (!SkipWhitespace(&data))
    return NULL;

  // We need the start of a value here now...
  JSONValue *value = JSONValue::Parse(&data);
  if (value == NULL)
    return NULL;
  
  // Can be white space now and should be at the end of the string then...
  if (SkipWhitespace(&data))
  {
    delete value;
    return NULL;
  }
  
  // We're now at the end of the string
  return value;
}

/**
 * Turns the passed in JSONValue into a JSON encode string
 *
 * @access public
 *
 * @param JSONValue* value The root value
 *
 * @return std::string Returns a JSON encoded string representation of the given value
 */
std::string JSON::Stringify(JSONValue *value)
{
  if (value != NULL)
    return value->Stringify();
  else
    return "";
}

/** 
 * Skips over any whitespace characters (space, tab, \r or \n) defined by the JSON spec
 *
 * @access protected
 *
 * @param char** data Pointer to a char* that contains the JSON text
 *
 * @return bool Returns true if there is more data, or false if the end of the text was reached
 */
bool JSON::SkipWhitespace(const char **data)
{
  while (**data != 0 && (**data == ' ' || **data == '\t' || **data == '\r' || **data == '\n'))
    (*data)++;
  
  return **data != 0;
}

/** 
 * Extracts a JSON String as defined by the spec - "<some chars>"
 * Any escaped characters are swapped out for their unescaped values
 *
 * @access protected
 *
 * @param char** data Pointer to a char* that contains the JSON text
 * @param std::string& str Reference to a std::string to receive the extracted string
 *
 * @return bool Returns true on success, false on failure
 */
bool JSON::ExtractString(const char **data, std::string &str)
{
  size_t str_length = 0;
  str = "";
  
  while (**data != 0)
  {
    // Save the char so we can change it if need be
    char next_char = **data;
    
    // Escaping something?
    if (next_char == '\\')
    {
      // Move over the escape char
      (*data)++;
      
      // Deal with the escaped char
      switch (**data)
      {
        case '"': next_char = '"'; break;
        case '\\' :next_char = '\\'; break;
        case '/': next_char = '/'; break;
        //case 'b': next_char = '\b'; break;
        case 'b': next_char = ' '; break;
        //case 'f': next_char = '\f'; break;
        case 'f': next_char = ' '; break;
        //case 'n': next_char = '\n'; break;
        case 'n': next_char = ' '; break;
        //case 'r': next_char = '\r'; break;
        case 'r': next_char = ' '; break;
        //case 't': next_char = '\t'; break;
        case 't': next_char = ' '; break;
        case 'u':
        {
          // We need 5 chars (4 hex + the 'u') or its not valid
          if (strlen(*data) < 5)
            return false;
          
          // Deal with the chars
          next_char = 0;
          for (int i = 0; i < 4; i++)
          {
            // Do it first to move off the 'u' and leave us on the 
            // final hex digit as we move on by one later on
            (*data)++;
            
            next_char <<= 4;
            
            // Parse the hex digit
            if (**data >= '0' && **data <= '9')
              next_char |= (**data - '0');
            else if (**data >= 'A' && **data <= 'F')
              next_char |= (10 + (**data - 'A'));
            else if (**data >= 'a' && **data <= 'f')
              next_char |= (10 + (**data - 'a'));
            else
            {
              // Invalid hex digit = invalid JSON
              return false;
            }
          }
          break;
        }
        
        // By the spec, only the above cases are allowed
        default:
          return false;
      }
    }
    
    // End of the string?
    else if (next_char == '"')
    {
      (*data)++;
      str.reserve(); // Remove unused capacity
      return true;
    }
    
    // Disallowed char?
    else if (next_char < ' ' && next_char != '\t')
    {
      // SPEC Violation: Allow tabs due to real world cases
      return false;
    }
    
    // String will be one longer - do it before memory size check
    str_length++;
    
    // Need more memory?
    if (str_length > str.capacity())
    {
      str_length += 256;
      str.reserve(str_length);
    }
    
    // Add the next char
    str += next_char;
    
    // Move on
    (*data)++;
  }
  
  // If we're here, the string ended incorrectly
  return false;
}

/** 
 * Parses some text as though it is an integer
 *
 * @access protected
 *
 * @param char** data Pointer to a char* that contains the JSON text
 *
 * @return int Returns the int value of the number found
 */
int JSON::ParseInt(const char **data)
{
  int integer = 0;
  while (**data != 0 && **data >= '0' && **data <= '9')
    integer = integer * 10 + (*(*data)++ - '0');
  return integer;
}
