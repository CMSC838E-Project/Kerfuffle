#include <stdio.h>
#include <stdlib.h>
#include "values.h"
#include "print.h"
#include "errors.h"

void default_type_handler(val_t e, val_t actual)
{
  int val = val_unwrap_int(e);
  printf("Error: expected ");
  print_str(val_unwrap_str(e));
  printf(" but got ");
  print_result(actual);
  printf("!\n");
  exit(1);
}

void raise_error_type(val_t e, val_t actual)
{
  return type_error_handler(e, actual);
}