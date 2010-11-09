#include "script_detector.h"
#include <iostream>
#include <cstring>
#include "utf8.h"

namespace inagist_classifiers {

ScriptDetector::ScriptDetector() {
  memset(m_buffer, '\0', SD_MAX_BUFFER_LEN);
}

ScriptDetector::~ScriptDetector() {
  Clear();
  memset(m_buffer, '\0', SD_MAX_BUFFER_LEN);
}

int ScriptDetector::Init() {
  Clear();
  return 0;
}

int ScriptDetector::Clear() {
  m_script_map.clear();
  m_buffer[0] = '\0';
  return 0;
}

int ScriptDetector::DetectScript(const std::string& text, std::set<std::string>& scripts) {
  if (text.length() < 1)
    return -1;  

  strcpy(m_buffer, text.c_str());
  char* ptr = m_buffer;
  char* end = strchr(m_buffer, '\0');
  if (!ptr || !end)
    return -1;

  int code_point = 0;
  std::string script;

  while (ptr && ptr < end) {
    try {
      code_point = utf8::next(ptr, end);
      if (DetectScript(code_point, script) >= 0)
        scripts.insert(script); 
    } catch (...) {
      ptr++;
    }
  }
  ptr = NULL;
  end = NULL;
  return 0;
}

// this function compares the input the code point ranges and
// populates output parameter with 2 letter script identifier
int ScriptDetector::DetectScript(int code_point, std::string &script) {

  // TODO (balaji) will optimize later.
  // if its only few languages, we can create fixed array based hash
  // else something on the lines of BST

  if (code_point >= 0 && code_point <= 127) {
      script = "en"; //Basic
      return 0;
  } else if (code_point >= 128 && code_point <= 879) {
    if (code_point >= 128 && code_point <= 255) {
      script = "en"; // Latin-1
    } else if (code_point >= 256 && code_point <= 383) {
      script = "en"; //Latin
    } else if (code_point >= 384 && code_point <= 591) {
      script = "en"; //Latin
    } else if (code_point >= 592 && code_point <= 687) {
      script = "en"; //IPA
    } else if (code_point >= 688 && code_point <= 767) {
      script = "en"; //Spacing
    } else if (code_point >= 768 && code_point <= 879) {
      script = "en"; //Combining
    }
    return 0;
  }
  else if (code_point >= 880 && code_point <= 1023) {
    script = "el"; //Greek
  } else if (code_point >= 1024 && code_point <= 1279) {
    script = "ru"; //Cyrillic
  } else if (code_point >= 1328 && code_point <= 1423) {
    script = "hy"; //Armenian
  } else if (code_point >= 1424 && code_point <= 1535) {
    script = "he"; //Hebrew
  } else if (code_point >= 1536 && code_point <= 1791) {
    script = "ar"; //Arabic
  } else if (code_point >= 1792 && code_point <= 1871) {
    script = "arc"; //Syriac
  } else if (code_point >= 1920 && code_point <= 1983) {
    script = "dv"; //Thaana
  } else if (code_point >= 2304 && code_point <= 2431) {
    script = "hi"; //Devanagari
  } else if (code_point >= 2432 && code_point <= 2559) {
    script = "bn"; //Bengali
  } else if (code_point >= 2560 && code_point <= 2687) {
    script = "pa"; //Gurmukhi
  } else if (code_point >= 2688 && code_point <= 2815) {
    script = "gu"; //Gujarati
  } else if (code_point >= 2816 && code_point <= 2943) {
    script = "or"; //Oriya
  } else if (code_point >= 2944 && code_point <= 3071) {
    script = "ta"; //Tamil
  } else if (code_point >= 3072 && code_point <= 3199) {
    script = "te"; //Telugu
  } else if (code_point >= 3200 && code_point <= 3327) {
    script = "kn"; //Kannada
  } else if (code_point >= 3328 && code_point <= 3455) {
    script = "ml"; //Malayalam
  } else if (code_point >= 3456 && code_point <= 3583) {
    script = "si"; //Sinhala
  } else if (code_point >= 3584 && code_point <= 3711) {
    script = "th"; //Thai
  } else if (code_point >= 3712 && code_point <= 3839) {
    script = "lo"; //Lao
  } else if (code_point >= 3840 && code_point <= 4095) {
    script = "bo"; //Tibetan
  } else if (code_point >= 4096 && code_point <= 4255) {
    script = "my"; //Myanmar
  } else if (code_point >= 4256 && code_point <= 4351) {
    script = "ka"; //Georgian
  } else if (code_point >= 4352 && code_point <= 4607) {
    script = "ko"; //Hangul
  } else if (code_point >= 4608 && code_point <= 4991) {
    script = "ti"; //Ethiopic
  } else if (code_point >= 5024 && code_point <= 5119) {
    script = "chr"; //Cherokee
  } else if (code_point >= 5120 && code_point <= 5759) {
    script = "cr"; //Unified
  } else if (code_point >= 5760 && code_point <= 5791) {
    script = "ogam"; //Ogham
  } else if (code_point >= 5792 && code_point <= 5887) {
    script = "runr"; //Runic
  } else if (code_point >= 6016 && code_point <= 6143) {
    script = "km"; //Khmer
  } else if (code_point >= 6144 && code_point <= 6319) {
    script = "mn"; //Mongolian
  } else if (code_point >= 7680 && code_point <= 7935) {
    script = "la"; //Latin
  } else if (code_point >= 7936 && code_point <= 8191) {
    script = "el"; //Greek
  } else if (code_point >= 8192 && code_point <= 8303) {
    script = "en"; //General
  } else if (code_point >= 8304 && code_point <= 8351) {
    script = "en"; //Superscripts
  } else if (code_point >= 8352 && code_point <= 8399) {
    script = "en"; //Currency
  } else if (code_point >= 8400 && code_point <= 8447) {
    script = "en"; //Combining
  } else if (code_point >= 8448 && code_point <= 8527) {
    script = "en"; //Letterlike
  } else if (code_point >= 8528 && code_point <= 8591) {
    script = "en"; //Number
  } else if (code_point >= 8592 && code_point <= 8703) {
    script = "en"; //Arrows
  } else if (code_point >= 8704 && code_point <= 8959) {
    script = "en"; //Mathematical
  } else if (code_point >= 8960 && code_point <= 9215) {
    script = "en"; //Miscellaneous
  } else if (code_point >= 9216 && code_point <= 9279) {
    script = "en"; //Control
  } else if (code_point >= 9280 && code_point <= 9311) {
    script = "en"; //Optical
  } else if (code_point >= 9312 && code_point <= 9471) {
    script = "en"; //Enclosed
  } else if (code_point >= 9472 && code_point <= 9599) {
    script = "en"; //Box
  } else if (code_point >= 9600 && code_point <= 9631) {
    script = "en"; //Block
  } else if (code_point >= 9632 && code_point <= 9727) {
    script = "en"; //Geometric
  } else if (code_point >= 9728 && code_point <= 9983) {
    script = "en"; //Miscellaneous
  } else if (code_point >= 9984 && code_point <= 10175) {
    script = "en"; //Dingbats
  } else if (code_point >= 10240 && code_point <= 10495) {
    script = "en"; //Braille
  } else if (code_point >= 11904 && code_point <= 12031) {
    script = "zh"; //CJK
  } else if (code_point >= 12032 && code_point <= 12255) {
    script = "zh"; //Kangxi
  } else if (code_point >= 12272 && code_point <= 12287) {
    script = "en"; //Ideographic
  } else if (code_point >= 12288 && code_point <= 12351) {
    script = "zh"; //CJK
  } else if (code_point >= 12352 && code_point <= 12447) {
    script = "jp"; //Hiragana
  } else if (code_point >= 12448 && code_point <= 12543) {
    script = "jp"; //Katakana
  } else if (code_point >= 12544 && code_point <= 12591) {
    script = "zh"; //Bopomofo
  } else if (code_point >= 12592 && code_point <= 12687) {
    script = "ko"; //Hangul
  } else if (code_point >= 12688 && code_point <= 12703) {
    script = "jp"; //Kanbun
  } else if (code_point >= 12704 && code_point <= 12735) {
    script = "zh"; //Bopomofo
  } else if (code_point >= 12800 && code_point <= 13055) {
    script = "zh"; //Enclosed
  } else if (code_point >= 13056 && code_point <= 13311) {
    script = "zh"; //CJK
  } else if (code_point >= 13312 && code_point <= 19893) {
    script = "zh"; //CJK
  } else if (code_point >= 19968 && code_point <= 40959) {
    script = "zh"; //CJK
  } else if (code_point >= 40960 && code_point <= 42127) {
    script = "zh"; //Yi
  } else if (code_point >= 42128 && code_point <= 42191) {
    script = "zh"; //Yi
  } else if (code_point >= 44032 && code_point <= 55203) {
    script = "ko"; //Hangul
  } else if (code_point >= 55296 && code_point <= 56191) {
    script = "zh"; //High
  } else if (code_point >= 56192 && code_point <= 56319) {
    script = "zh"; //High
  } else if (code_point >= 56320 && code_point <= 57343) {
    script = "zh"; //Low
  } else if (code_point >= 57344 && code_point <= 63743) {
    script = "zh"; //Private
  } else if (code_point >= 63744 && code_point <= 64255) {
    script = "zh"; //CJK
  } else if (code_point >= 64256 && code_point <= 64335) {
    script = "en"; //Alphabetic
  } else if (code_point >= 64336 && code_point <= 65023) {
    script = "ar"; //Arabic
  } else if (code_point >= 65056 && code_point <= 65071) {
    script = "zh"; //Combining
  } else if (code_point >= 65072 && code_point <= 65103) {
    script = "zh"; //CJK
  } else if (code_point >= 65104 && code_point <= 65135) {
    script = "en"; //Small
  } else if (code_point >= 65136 && code_point <= 65278) {
    script = "ar"; //Arabic
  } else if (code_point >= 65279 && code_point <= 65279) {
    script = "en"; //Specials
  } else if (code_point >= 65280 && code_point <= 65519) {
    script = "en"; //Halfwidth
  } else if (code_point >= 65520 && code_point <= 65533) {
    script = "en"; //Specials
  } else {
    return -1;
  }

  if (m_script_map.find(script) != m_script_map.end()) {
    m_script_map[script] += 1;
  } else {
    m_script_map[script] = 1; 
  }

  return 1;
}

int ScriptDetector::GetMaxScript(std::string& script) {
  // currently sending the first script with more than 5 characters
  // not necessarily the max
  if (m_script_map.empty()) {
    script = "xx";
    return -1;
  }

  int ret_value = 0;
  for (m_script_map_iter = m_script_map.begin(); m_script_map_iter != m_script_map.end(); m_script_map_iter++) {
    if ((ret_value = m_script_map_iter->second) > 9) {
      script = m_script_map_iter->first;
      break;
    }
  }

  if (ret_value <= 9)
    script = "xx";

  return ret_value;
}

int ScriptDetector::GetScripts(std::set<std::string>& scripts) {
  for (m_script_map_iter = m_script_map.begin(); m_script_map_iter != m_script_map.end(); m_script_map_iter++)
    scripts.insert(m_script_map_iter->first);
  return 0;
}

}
