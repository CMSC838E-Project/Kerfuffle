#ifndef ERRORS_H
#define ERRORS_H

#include "values.h"

void (*type_error_handler)(val_t, val_t);
void default_type_handler(val_t e, val_t actual);

#endif