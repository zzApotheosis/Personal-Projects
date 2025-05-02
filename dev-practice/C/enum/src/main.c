#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>

#define NUM_DAYS 7
#define NUM_MONTHS 12

enum day{Sun = 1, Mon, Tue, Wed, Thur, Fri, Sat};
enum month{Jan = 1, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec};

void init() {
    srand(time(NULL));
}

unsigned char * get_day_name(unsigned char day) {
    switch(day) {
    case Sun:
        return("Sunday");
    case Mon:
        return("Monday");
    case Tue:
        return("Tuesday");
    case Wed:
        return("Wednesday");
    case Thur:
        return("Thursday");
    case Fri:
        return("Friday");
    case Sat:
        return("Saturday");
    default:
        return("wtf");
    }
}

unsigned char * get_month_name(unsigned char month) {
    switch(month) {
    case Jan:
        return("January");
    case Feb:
        return("February");
    case Mar:
        return("March");
    case Apr:
        return("April");
    case May:
        return("May");
    case Jun:
        return("June");
    case Jul:
        return("July");
    case Aug:
        return("August");
    case Sep:
        return("September");
    case Oct:
        return("October");
    case Nov:
        return("November");
    case Dec:
        return("December");
    }
}

int main(int argc, char * argv[]) {
    init();
    enum day day = rand() % NUM_DAYS + 1;
    enum month birth_month = Jul;

    fprintf(stdout, "Looks like we got a %s!\n", get_day_name(day));
    fprintf(stdout, "What month is my birthday? Definitely not %s\n", get_month_name(birth_month));
    
    return(EXIT_SUCCESS);
}
