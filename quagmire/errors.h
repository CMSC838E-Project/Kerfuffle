#ifndef ERRORS_H
#define ERRORS_H

void default_type_handler(val_t, val_t);
extern void (*type_error_handler)(val_t, val_t);

#endif