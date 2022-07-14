#pragma once

#include <vector>

class Function
{
  friend class Trie;

public:
  std::string name;
  std::vector<std::string> argument_types;
  bool is_variadic;
  Function(std::string n, std::vector<std::string> a, bool v) : name(n), argument_types(a), is_variadic(v){};
};

inline bool operator==(const Function &lhs, const Function &rhs)
{
  return (lhs.name == rhs.name) && (lhs.argument_types == rhs.argument_types) && (lhs.is_variadic == rhs.is_variadic);
}

inline bool operator!=(const Function &lhs, const Function &rhs) { return !operator==(lhs, rhs); }
inline bool operator<(const Function &lhs, const Function &rhs)
{ /* do actual comparison */
  // Perform comparison only for the names
  return lhs.name < rhs.name;
}
inline bool operator>(const Function &lhs, const Function &rhs) { return operator<(rhs, lhs); }
inline bool operator<=(const Function &lhs, const Function &rhs) { return !operator>(lhs, rhs); }
inline bool operator>=(const Function &lhs, const Function &rhs) { return !operator<(lhs, rhs); }

std::ostream &operator<<(std::ostream &os, const Function &value)
{
  os << value.name;
  return os;
}

std::ostream &operator<<(std::ostream &os, const std::vector<Function> &v)
{
  os << "{";
  for (size_t i = 0; i < v.size(); ++i)
  {
    if (i != 0)
    {
      os << ", ";
    }
    os << v[i].name;
  }
  os << "}";
  return os;
}