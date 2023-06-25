/*
       Copyright 2008-Present, California Institute of Technology.
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/
/**
 * @author Stirling Algermissen {Stirling.Algermissen@jpl.nasa.gov}
 */

#ifndef M2020DB_H
#define M2020DB_H
#include <mysql_connection.h>
#include <mysql_driver.h>
#include <cppconn/driver.h>
#include <cppconn/exception.h>
#include <cppconn/resultset.h>
#include <cppconn/statement.h>
#include <cppconn/prepared_statement.h>
#include "M2020Exception.h"

using namespace std;

#define MAX_QUERY_LEN 500

class M2020Db {
public:
  //class constructor
  M2020Db(); 

  //connection & disconnect
  int       openConnection(const char *host, const char *db, int port, const char *user, const char *pass); //connect to the database
  bool      disconnect(); //disconnect from the database
  sql::Connection *con;  

private:
  sql::Driver *driver;

};

char *dberror(int errorcode); //return a string for this error message

#endif
