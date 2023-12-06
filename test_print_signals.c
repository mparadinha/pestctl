#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <stdbool.h>

const char* signal_names[] = {
    "",
    "SIGHUP",    // 1
    "SIGINT",    // 2
    "SIGQUIT",   // 3
    "SIGILL",    // 4
    "SIGTRAP",   // 5
    "SIGABRT",   // 6
    "SIGBUS",    // 7
    "SIGFPE",    // 8
    "SIGKILL",   // 9
    "SIGUSR1",   // 10
    "SIGSEGV",   // 11
    "SIGUSR2",   // 12
    "SIGPIPE",   // 13
    "SIGALRM",   // 14
    "SIGTERM",   // 15
    "SIGSTKFLT", // 16
    "SIGCHLD",   // 17
    "SIGCONT",   // 18
    "SIGSTOP",   // 19
    "SIGTSTP",   // 20
    "SIGTTIN",   // 21
    "SIGTTOU",   // 22
    "SIGURG",    // 23
    "SIGXCPU",   // 24
    "SIGXFSZ",   // 25
    "SIGVTALRM", // 26
    "SIGPROF",   // 27
    "SIGWINCH",  // 28
    "SIGPOOL",   // 29
    "SIGPWR",    // 30
    "SIGSYS",    // 31
};

void signal_handler(int sig, siginfo_t* info, void* ctx) {
    printf("received %s (sig=%d)\n", signal_names[sig], sig);
}

int main() {
    struct sigaction action = {
        .sa_sigaction = signal_handler,
        .sa_flags = SA_SIGINFO,
    };
    for (int sig = 1; sig < 32; sig++) sigaction(sig, &action, NULL);

    while(true) {
        printf("waiting for signals...\n");
        pause();
    }
}
