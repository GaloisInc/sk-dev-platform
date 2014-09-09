#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <selinux/selinux.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

/*
 * Program corresponding to the testapp.lsr Lobster specification
 *
 * This is an SELinux-aware program, needing to know something about
 * SELinux types.
 */

void die(char* msg) {
  int r;

  r = ((printf("%s\n", msg) < 0) ? 2 : 1);

  exit(r);
}

void print_context() {
  security_context_t s;
  if (getcon(&s)) { die("can't allocate security_context_t"); }
  if (printf("%s", s) < 0) { die("can't print"); }
  freecon(s);
}

/*
 * Paranoid wrapper of a setexeccon() call.
 */
void set_exec_context(security_context_t s) {
  int i;
  security_context_t c;
  if (getexeccon(&c)) { die("can't get exec context"); }
  if (c == NULL) {
    /* SELinux does not say anything, do what policy says */
    if (printf("Exec context is default\n") < 0) { die("can't print"); }
  }
  else {
    /* We will explicitly override the policy, we have already
     * done a setexeccon()
     */
    if (printf("Exec context: %s\n", c) < 0) { die("can't print"); }
    freecon(c);
  }
  if (!security_check_context(s)) { die("invalid security context"); }
  /* Set the executable secrity context */
  i = setexeccon(s);
  if (i < 0) {
    printf("setexeccon return code = %d\n", i);
    printf("errno = %d (\"%s\")\n", errno, strerror(errno));
    die("can't set exec context");
  }
  if (getexeccon(&c)) { die("can't get exec context"); }
  if (c == NULL) {
    if (printf("Exec context is default\n") < 0) { die("can't print"); }
  }
  else {
    if (printf("Exec context: %s\n", c) < 0) { die("can't print"); }
    freecon(c);
  }
}

int producer() {
    printf("Producer: ");
    print_context();
    printf("\n");
    return 0;
}

int consumer() {
    printf("Consumer: ");
    print_context();
    printf("\n");
    return 0;
}

/*
 * This main program can be any of { controller, producer, consumer }
 * After determining which of these three is wanted, we exec()
 */

int main(int argc, char** argv) {
  int i;
  pid_t p;

  if (argc <= 0) { die("no executable name"); }    
  if (printf("Starting %s\n", argv[0]) < 0) { die("can't print"); }

  /* The is_selinux_enabled() call requires permission to read the proc file system
  i = is_selinux_enabled();
  printf("SELinux enabled: %d\n", i);
  if (i != 1) { die("SELinux not enabled"); }
  */

  i = is_selinux_mls_enabled();
  printf("MLS enabled: %d\n", i);

  i = security_policyvers();
  printf("Policy version: %d\n", i);

  /* This is the code that the master will use */
  if (argc <= 1) {
    printf("Master: ");
    print_context(); /* Which security context we are in */
    printf("\n");
    p = fork();
    if (p == -1) { die("can't fork"); }
    if (p == 0) {
      /* Set up new security context for the producer
       * type_transitions are functions, not relations, so cannot use
       * type transitions to support master, producer, and executable
       * using the same executable file.
       *
       * set_exec_context tells SELinux what context to use next time
       * an exec*() call is done.
       */
      set_exec_context("system_u:system_r:testapp_producer_t:s0-s0:c0.c1023");
      execl(argv[0], argv[0], "Producer", NULL);
      die("can't exec");
    }
    else {
      /* Set up new security context for the consumer */
      set_exec_context("system_u:system_r:testapp_consumer_t:s0");
      execl(argv[0], argv[0], "Consumer", NULL);
      die("can't exec");
    }
  }
  /* This is the code that the producer will use */
  else if (strcmp(argv[1],"Producer") == 0) {
    return producer();
  }
  /* This is the code that the consumer will use */
  else if (strcmp(argv[1],"Consumer") == 0) {
    return consumer();
  }
  else {
    die("bad role");
  }

  return 1;
}
