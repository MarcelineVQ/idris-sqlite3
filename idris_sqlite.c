// C-Side of the Idris sqlite bindings
// #include "idris_net.h"
#include <stdlib.h>
#include <stdio.h>
#include <sqlite3.h>

void* newptr(){
    return malloc(sizeof(void*));
}

void* deref(void** ptr){
  return *ptr;
}

void ptr_free(void* ptr){
  free(ptr);
}

char* getString(void* str) {
    return (char*)str;
}

void* mkString(char* str) {
    return (void*)str;
}

void sqlver(void) {
    printf("%s\n", sqlite3_libversion()); 
}

void* null(void) {
    return NULL;
}

int isNull(void* ptr) {
    return ptr==NULL;
}
