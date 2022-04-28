#include <stdio.h>
#include <stdlib.h>
#include "values.h"

const char* get_type_actual(val_t);
const char* get_type_expect(val_t);

void raise_error_type(val_t e, val_t actual)
{
  int val = val_unwrap_int(e);
  printf("Error: expected %s but got %s!\n", get_type_expect(val), get_type_actual(actual));
  exit(1);
}

const char* get_type_actual(val_t actual)
{
  switch (val_typeof(actual)) 
  {
    case T_INT:
      return "Integer";
    case T_BOOL:
      return "Boolean";
    case T_CHAR:
      return "Char";
    case T_EMPTY:
    case T_CONS:
      return "List";
    case T_VECT:    
      return "Vector";
    case T_STR:
      return "String";
    case T_INVALID:
      return "Invalid";
  }
  return "Unknown";
}

const char* get_type_expect(val_t e)
{
  switch (e)
  {
    case 1:
      return "Integer";
    case 2:
      return "Char";
    case 3:
      return "String";
    case 4:
      return "Boolean";
    case 5:
      return "List";
    case 6:
      return "Vector";
    case 7:
      return "Union[...]";
    default:
      return "Unknown";
  }
}