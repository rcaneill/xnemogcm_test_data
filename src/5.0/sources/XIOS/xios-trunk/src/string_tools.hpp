#ifndef __STRING_TOOLS_HPP__
#define __STRING_TOOLS_HPP__

#include <string>
#include <regex>
#include <vector>

namespace xios
{
  std::vector<std::string> splitRegex(const std::string& input, const std::string& regex) ;
  string strTrim(const std::string& input) ;

  inline std::vector<std::string> splitRegex(const std::string& input, const std::string& regex)
  {
      // passing -1 as the submatch index parameter performs splitting
      std::regex re(regex);
      std::regex_token_iterator<std::string::const_iterator>
          first{input.begin(), input.end(), re, -1},
          last;
      return {first, last}; 
  }

  inline string strTrim(const std::string& input)
  {
    return std::regex_replace(input, std::regex("^ +| +$"), "$1");
  }

}
#endif
