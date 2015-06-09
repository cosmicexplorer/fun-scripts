#ifndef TL_H
#define TL_H

#ifndef __linux__
#error PLATFORM NOT SUPPORTED: LINUX ONLY FOR NOW
#endif /* ifndef __linux__ */

/*
 * WARNING: TL_EXPOSE_VAR variables MUST have a globally unique name among the
 * code getting compiled (not headers n stuff), otherwise it will insert
 * tl_log_reads and whatever where they shouldn't be! luckily, we can check for
 * that.
 */

// TODO: add structs/classes later

/* PRIMITIVE VARIABLE FUNCTIONS */
// logs when variable read from, and value
// along with thread and pid of whoever read from it
// void tl_log_read(char *name);

// logs when variable written to, and value
// along with the thread and process id of whoever wrote to it
// void tl_log_write(char *name);

/* STRUCT VARIABLE FUNCTIONS */
// logs when a method is called on some struct/class variable. since it is a
// struct or class, we do not know whether or not calling a method on it will
// access the variable or modify its fields.
// void tl_log_access(char *name);

/*
 * TODO: Append log_access functions for all types of all variables. We know
 * what the variable's type is, since we have to add TL_EXPOSE_VAR to the
 * _declaration_; as a result, we can generate functions that log reads and
 * writes, and return the value of the variable.
 *
 * Why is this useful? It allows us to do insane things like:j
 *
 * TL_EXPOSE_VAR struct type_t a; // adds to our gperf database while parsing
 * (a.getStuff()).bind(); -> ((log_access_type_t("a", a)).getStuff()).bind();
 *
 * type_t & log_access_type_t(char * name, type_t & t) {
 *   tl_log_access(name);
 *   return t;
 * }
 */

#endif /* ifndef TL_H */
