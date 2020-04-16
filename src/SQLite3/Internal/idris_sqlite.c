// C-Side of the Idris sqlite bindings
// #include "idris_net.h"
#include <sqlite3.h>
#include <stdlib.h>

void* sqlite_db_handle(void){
    sqlite3* db;
    return db;
}

void* sqlite_stmt_handle(void){
    sqlite3_stmt* stmt;
    return stmt;
}


// int sqlite3_prepare_v2(
//   sqlite3 *db,            /* Database handle */
//   const char *zSql,       /* SQL statement, UTF-8 encoded */
//   int nByte,              /* Maximum length of zSql in bytes. */
//   sqlite3_stmt **ppStmt,  /* OUT: Statement handle */
//   const char **pzTail     /* OUT: Pointer to unused portion of zSql */
// );

void* null(void) {
    return NULL;
}

int isNull(void* ptr) {
    return ptr==NULL;
}
