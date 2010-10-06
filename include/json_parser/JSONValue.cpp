/*
 * JSONValue.cpp
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vector>
#include <string>
#include <sstream>
#include <iostream>
#include <math.h>

#include "JSONValue.h"

// Macros to free an array/object
#define FREE_ARRAY(x) { JSONArray::iterator iter; for (iter = x.begin(); iter != x.end(); iter++) { delete *iter; } }
#define FREE_OBJECT(x) { JSONObject::iterator iter; for (iter = x.begin(); iter != x.end(); iter++) { delete (*iter).second; } }

/**
 * Parses a JSON encoded value to a JSONValue object
 *
 * @access protected
 *
 * @param char** data Pointer to a char* that contains the data
 *
 * @return JSONValue* Returns a pointer to a JSONValue object on success, NULL on error
 */
JSONValue *JSONValue::Parse(const char **data)
{
  // Is it a string?
  if (**data == '"')
  {
    std::string str;
    if (!JSON::ExtractString(&(++(*data)), str))
      return NULL;
    else
      return new JSONValue(str);
  }
  
  // Is it a boolean?
  else if ((strlen(*data) >= 4 && strncasecmp(*data, "true", 4) == 0) || (strlen(*data) >= 5 && strncasecmp(*data, "false", 5) == 0))
  {
    bool value = strncasecmp(*data, "true", 4) == 0;
    (*data) += value ? 4 : 5;
    return new JSONValue(value);
  }
  
  // Is it a null?
  else if (strlen(*data) >= 4 && strncasecmp(*data, "null", 4) == 0)
  {
    (*data) += 4;
    return new JSONValue();
  }
  
  // Is it a number?
  else if (**data == '-' || (**data >= '0' && **data <= '9'))
  {
    // Negative?
    bool neg = **data == '-';
    if (neg) (*data)++;

    double number = 0.0;

    // Parse the whole part of the number - only if it wasn't 0
    if (**data == '0')
      (*data)++;
    else if (**data >= '1' && **data <= '9')
      number = (double)JSON::ParseInt(data);
    else
      return NULL;
    
    // Could be a decimal now...
    if (**data == '.')
    {
      (*data)++;

      // Not get any digits?
      if (!(**data >= '0' && **data <= '9'))
        return NULL;
      
      // Find the decimal and sort the decimal place out
      double decimal = (double)JSON::ParseInt(data);
      while((int)decimal > 0) decimal /= 10.0f;

      // Save the number
      number += decimal;
    }

    // Could be an exponent now...
    if (**data == 'E' || **data == 'e')
    {
      (*data)++;

      // Check signage of expo
      bool neg_expo = false;
      if (**data == '-' || **data == '+')
      {
        neg_expo = **data == '-';
        (*data)++;
      }
      
      // Not get any digits?
      if (!(**data >= '0' && **data <= '9'))
        return NULL;

      // Sort the expo out
      int expo = JSON::ParseInt(data);
      for (int i = 0; i < expo; i++)
        number = neg_expo ? (number / 10.0) : (number * 10);
    }

    // Was it neg?
    if (neg) number *= -1;

    return new JSONValue(number);
  }

  // An object?
  else if (**data == '{')
  {
    JSONObject object;
    
    (*data)++;
  
    while (**data != 0)
    {
      // Whitespace at the start?
      if (!JSON::SkipWhitespace(data))
      {
        FREE_OBJECT(object);
        return NULL;
      }
      
      // Special case - empty object
      if (object.size() == 0 && **data == '}')
      {
        (*data)++;
        return new JSONValue(object);
      }
      
      // We want a string now...
      std::string name;
      if (!JSON::ExtractString(&(++(*data)), name))
      {
        FREE_OBJECT(object);
        return NULL;
      }
      
      // More whitespace?
      if (!JSON::SkipWhitespace(data))
      {
        FREE_OBJECT(object);
        return NULL;
      }
      
      // Need a : now
      if (*((*data)++) != ':')
      {
        FREE_OBJECT(object);
        return NULL;
      }
      
      // More whitespace?
      if (!JSON::SkipWhitespace(data))
      {
        FREE_OBJECT(object);
        return NULL;
      }
      
      // The value is here      
      JSONValue *value = Parse(data);
      if (value == NULL)
      {
        FREE_OBJECT(object);
        return NULL;
      }
      
      // Add the name:value
      if (object.find(name) != object.end())
        delete object[name];
      object[name] = value;
      
      // More whitespace?
      if (!JSON::SkipWhitespace(data))
      {
        FREE_OBJECT(object);
        return NULL;
      }
      
      // End of object?
      if (**data == '}')
      {
        (*data)++;
        return new JSONValue(object);
      }
      
      // Want a , now
      if (**data != ',')
      {
        FREE_OBJECT(object);
        return NULL;
      }
      
      (*data)++;
    }
    
    // Only here if we ran out of data
    FREE_OBJECT(object);
    return NULL;
  }
  
  // An array?
  else if (**data == '[')
  {
    JSONArray array;
    
    (*data)++;
    
    while (**data != 0)
    {
      // Whitespace at the start?
      if (!JSON::SkipWhitespace(data))
      {
        FREE_ARRAY(array);
        return NULL;
      }
      
      // Special case - empty array
      if (array.size() == 0 && **data == ']')
      {
        (*data)++;
        return new JSONValue(array);
      }
      
      // Get the value
      JSONValue *value = Parse(data);
      if (value == NULL)
      {
        FREE_ARRAY(array);
        return NULL;
      }
      
      // Add the value
      array.push_back(value);
      
      // More whitespace?
      if (!JSON::SkipWhitespace(data))
      {
        FREE_ARRAY(array);
        return NULL;
      }
      
      // End of array?
      if (**data == ']')
      {
        (*data)++;
        return new JSONValue(array);
      }
      
      // Want a , now
      if (**data != ',')
      {
        FREE_ARRAY(array);
        return NULL;
      }
      
      (*data)++;
    }
    
    // Only here if we ran out of data
    FREE_ARRAY(array);
    return NULL;
  }
  
  // Ran out of possibilites, it's bad!
  else
  {
    return NULL;
  }
}

/** 
 * Basic constructor for creating a JSON Value of type NULL
 *
 * @access public
 */
JSONValue::JSONValue(/*NULL*/)
{
  type = JSONType_Null;
}

/** 
 * Basic constructor for creating a JSON Value of type String
 *
 * @access public
 *
 * @param char* m_char_value The string to use as the value
 */
JSONValue::JSONValue(const char *m_char_value)
{
  type = JSONType_String;
  string_value = std::string(m_char_value);
}

/** 
 * Basic constructor for creating a JSON Value of type String
 *
 * @access public
 *
 * @param std::string m_string_value The string to use as the value
 */
JSONValue::JSONValue(std::string m_string_value)
{
  type = JSONType_String;
  string_value = m_string_value;
}

/** 
 * Basic constructor for creating a JSON Value of type Bool
 *
 * @access public
 *
 * @param bool m_bool_value The bool to use as the value
 */
JSONValue::JSONValue(bool m_bool_value)
{
  type = JSONType_Bool;
  bool_value = m_bool_value;
}

/** 
 * Basic constructor for creating a JSON Value of type Number
 *
 * @access public
 *
 * @param double m_number_value The number to use as the value
 */
JSONValue::JSONValue(double m_number_value)
{
  type = JSONType_Number;
  number_value = m_number_value;
}

/** 
 * Basic constructor for creating a JSON Value of type Array
 *
 * @access public
 *
 * @param JSONArray m_array_value The JSONArray to use as the value
 */
JSONValue::JSONValue(JSONArray m_array_value)
{
  type = JSONType_Array;
  array_value = m_array_value;
}

/** 
 * Basic constructor for creating a JSON Value of type Object
 *
 * @access public
 *
 * @param JSONObject m_object_value The JSONObject to use as the value
 */
JSONValue::JSONValue(JSONObject m_object_value)
{
  type = JSONType_Object;
  object_value = m_object_value;
}

/** 
 * The destructor for the JSON Value object
 * Handles deleting the objects in the array or the object value
 *
 * @access public
 */
JSONValue::~JSONValue()
{
  if (type == JSONType_Array)
  {
    JSONArray::iterator iter;
    for (iter = array_value.begin(); iter != array_value.end(); iter++)
      delete *iter;
  }
  else if (type == JSONType_Object)
  {
    JSONObject::iterator iter;
    for (iter = object_value.begin(); iter != object_value.end(); iter++)
    {
      delete (*iter).second;
    }
  }
}

/** 
 * Checks if the value is a NULL
 *
 * @access public
 *
 * @return bool Returns true if it is a NULL value, false otherwise
 */
bool JSONValue::IsNull()
{
  return type == JSONType_Null;
}

/** 
 * Checks if the value is a String
 *
 * @access public
 *
 * @return bool Returns true if it is a String value, false otherwise
 */
bool JSONValue::IsString()
{
  return type == JSONType_String;
}

/** 
 * Checks if the value is a Bool
 *
 * @access public
 *
 * @return bool Returns true if it is a Bool value, false otherwise
 */
bool JSONValue::IsBool()
{
  return type == JSONType_Bool;
}

/** 
 * Checks if the value is a Number
 *
 * @access public
 *
 * @return bool Returns true if it is a Number value, false otherwise
 */
bool JSONValue::IsNumber()
{
  return type == JSONType_Number;
}

/** 
 * Checks if the value is an Array
 *
 * @access public
 *
 * @return bool Returns true if it is an Array value, false otherwise
 */
bool JSONValue::IsArray()
{
  return type == JSONType_Array;
}

/** 
 * Checks if the value is an Object
 *
 * @access public
 *
 * @return bool Returns true if it is an Object value, false otherwise
 */
bool JSONValue::IsObject()
{
  return type == JSONType_Object;
}

/** 
 * Retrieves the String value of this JSONValue
 * Use IsString() before using this method.
 *
 * @access public
 *
 * @return std::string Returns the string value
 */
std::string JSONValue::AsString()
{
  return string_value;
}

/** 
 * Retrieves the Bool value of this JSONValue
 * Use IsBool() before using this method.
 *
 * @access public
 *
 * @return bool Returns the bool value
 */
bool JSONValue::AsBool()
{
  return bool_value;
}

/** 
 * Retrieves the Number value of this JSONValue
 * Use IsNumber() before using this method.
 *
 * @access public
 *
 * @return double Returns the number value
 */
double JSONValue::AsNumber()
{
  return number_value;
}

/** 
 * Retrieves the Array value of this JSONValue
 * Use IsArray() before using this method.
 *
 * @access public
 *
 * @return JSONArray Returns the array value
 */
JSONArray JSONValue::AsArray()
{
  return array_value;
}

/** 
 * Retrieves the Object value of this JSONValue
 * Use IsObject() before using this method.
 *
 * @access public
 *
 * @return JSONObject Returns the object value
 */
JSONObject JSONValue::AsObject()
{
  return object_value;
}

/** 
 * Creates a JSON encoded string for the value with all necessary characters escaped
 *
 * @access public
 *
 * @return std::string Returns the JSON string
 */
std::string JSONValue::Stringify()
{
  std::string ret_string;
  
  switch (type)
  {
    case JSONType_Null:
      ret_string = "null";
      break;
    
    case JSONType_String:
      ret_string = StringifyString(string_value);
      break;
    
    case JSONType_Bool:
      ret_string = bool_value ? "true" : "false";
      break;
    
    case JSONType_Number:
    {
      if (isinf(number_value) || isnan(number_value))
        ret_string = "null";
      else
      {
        std::stringstream ss;
        ss << number_value;
        ret_string = ss.str();
      }
      break;
    }
    
    case JSONType_Array:
    {
      ret_string = "[";
      JSONArray::iterator iter = array_value.begin();
      while (iter != array_value.end())
      {
        ret_string += (*iter)->Stringify();
        
        // Not at the end - add a separator
        if (++iter != array_value.end())
          ret_string += ",";
      }
      ret_string += "]";
      break;
    }
    
    case JSONType_Object:
    {
      ret_string = "{";
      JSONObject::iterator iter = object_value.begin();
      while (iter != object_value.end())
      {
        ret_string += StringifyString((*iter).first);
        ret_string += ":";
        ret_string += (*iter).second->Stringify();
        
        // Not at the end - add a separator
        if (++iter != object_value.end())
          ret_string += ",";
      }
      ret_string += "}";
      break;
    }
  }

  return ret_string;
}

/** 
 * Creates a JSON encoded string with all required fields escaped
 * Works from http://www.ecma-internationl.org/publications/files/ECMA-ST/ECMA-262.pdf
 * Section 15.12.3.
 *
 * @access private
 *
 * @param std::string str The string that needs to have the characters escaped
 *
 * @return std::string Returns the JSON string
 */
std::string JSONValue::StringifyString(std::string str)
{
  std::string str_out = "\"";
  
  std::string::iterator iter = str.begin();
  while (iter != str.end())
  {
    char chr = *iter;

    if (chr == '"' || chr == '\\' || chr == '/')
    {
      str_out += '\\';
      str_out += chr;
    }
    else if (chr == '\b')
    {
      str_out += "\\b";
    }
    else if (chr == '\f')
    {
      str_out += "\\f";
    }
    else if (chr == '\n')
    {
      str_out += "\\n";
    }
    else if (chr == '\r')
    {
      str_out += "\\r";
    }
    else if (chr == '\t')
    {
      str_out += "\\t";
    }
    /*
    else if (chr < ' ')
    {
      str_out += "\\u";
      for (int i = 0; i < 4; i++)
      {
        int value = (chr >> 12) & 0xf;
        if (value >= 0 && value <= 9)
          str_out += (char)('0' + value);
        else if (value >= 10 && value <= 15)
          str_out += (char)('A' + (value - 10));
        chr <<= 4;
      }
    }*/
    else
    {
      str_out += chr;
    }
    
    iter++;
  }
  
  str_out += "\"";
  return str_out;
}
