#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#define	COB_LIB_EXPIMP
#include "libcob.h"
#include "coblocal.h"

void trim(char* str) {
	if(!str) return;
	
	ltrim(str);
	rtrim(str);
}

void rtrim(char* str) {
	int i;
	if(!str) return;

	for(i = strlen(str) - 1; i >= 0; i--) {
		if(str[i] == ' ') str[i] = 0x00;
		else break;
	}
}

/* CHECKME */
void ltrim(char* str) {
	int i, j;
	if(!str) return;

	for(i = 0; i < strlen(str); i++) {
		if(!str[i] == ' ') break;
	}

	if(i > 0) {
		for(j = 0; j < strlen(str) - i - 1; j++) {
			str[j] = str[j + i];
		}
		str[strlen(str) - i - 1] = 0x00;
	}
}

void all_to_upper(char* str) {
	int i;
	if(!str) return;

	for(i = 0; i < strlen(str); i++) {
		str[i] = toupper(str[i]);
	}
}
