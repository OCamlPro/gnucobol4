#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define	COB_LIB_EXPIMP
#include "libcob.h"
#include "coblocal.h"

struct anim_field empty_anim_field = {"Not found", NULL, NULL, 9, 0, "-", NULL, NULL, {0, 0}};

static char* get_subscript(char* str) {
	int i, pos = 0;
	int flag_r_brace = 0;
	char* subscripts = NULL;

	for(i = strlen(str); i > 0; i--) {
		if(str[i] == ')') flag_r_brace = 1;
		if(str[i] == '(' && flag_r_brace) {
			flag_r_brace = 0;
			pos = i;
		}
	}

	if(pos > 0) {
		subscripts = (char*) calloc(1, strlen(str) - pos + 1);
		strcpy(subscripts, str + pos);
		str[pos] = 0x00; /* Cut subscripts from str */
	}

	return subscripts;
}

static int calculate_subscript_shift(char* str, struct anim_field* af) {
	int i;
	//int pos = 0;
	int r_brace_pos;
	int shift = 0;
	int subscript_tmp;
	char tmp_str[25];
	
	/* Check if we've got any subscript expressions and count them */
	i = strlen(str) - 1;
	if(str[i] == ')') {
		for(i; i >= 0; i--) {
			if(str[i] == ')') r_brace_pos = i;
			if(str[i] == '(') {
				memset(&tmp_str, 0x00, 25);
				memcpy(&tmp_str, str + i + 1, r_brace_pos - i - 1);
				if(!af || !af->flags.occurs_flag) return 0;
				subscript_tmp = atoi(tmp_str);
				if(subscript_tmp > 0 && subscript_tmp <= af->flags.occurs_max)
					shift = shift + (subscript_tmp - 1) * af->size;
				else return 0;
				af = (struct anim_field*) af->parent;
			}
		}
	}

	return shift;
}

static char* get_parent_base_data(anim_field* af) {
	while(af->parent) {
		af = af->parent;
	}

	if(af->base_field) return (char*) af->base_field;
	if(af->src_field && af->src_field->data) return af->src_field->data;

	return NULL;
}

void anidata(anim_field* caf, anim_field* cafl, anim_field* cafg, interface_block* ib, char* orig_program_id, char* mode, char* f_name) {
	cob_field_attr tmp_attr = {0x22, 0, 0, 0x0000, NULL}; /* 0x22 - ALPHANUMERIC_ALL */ 
	char dst_data[280];
	char *f_name_cpy;
	char *caf_field_name_cpy = NULL, *cafl_field_name_cpy = NULL, *cafg_field_name_cpy = NULL;
	char* subscript;
	char* field_data;
	int subscript_shift = 0;
	int copy_size;

	cob_field dst = {280, (unsigned char*) &dst_data, &tmp_attr};
	char work[12];
	if((!caf && !cafl && !cafg) || !ib || !f_name) {
		caf = &empty_anim_field;
		sprintf(work, "%02d", caf->usage);
		memcpy(ib->dtf_usage, work, 2);
		sprintf(work, "%03d", 0);
		memcpy(ib->dtf_length, work, 3);
		memset(ib->dtf_value, 0x20, 280);
		memcpy(ib->dtf_value, "Not found", caf->size);
		return;
	}

	/* Copy & Trim f_name */
	f_name_cpy = cob_strdup(f_name);
	rtrim(f_name_cpy);

	/* All to upper case for comparing */
	all_to_upper(f_name_cpy);

	/* Has Subscript? Cut it... */
	subscript = get_subscript(f_name_cpy);
		
	 /* Search working-storage variables */
	while (caf && caf->field_name && orig_program_id) {
		/* ignore case differences */
		caf_field_name_cpy = cob_strdup(caf->field_name);
		all_to_upper(caf_field_name_cpy);
		
		if(strncmp(caf_field_name_cpy, f_name_cpy, strlen(caf_field_name_cpy)) == 0 && strncmp(caf->program_id, orig_program_id, strlen(caf->program_id)) == 0) {
			if(subscript) subscript_shift = calculate_subscript_shift(subscript, caf);
			
			sprintf(work, "%02d", caf->usage);
			memcpy(ib->dtf_usage, work, 2);
			sprintf(work, "%03d", caf->size);
			memcpy(ib->dtf_length, work, 3);
			memset(ib->dtf_value, 0x20, 280);
			if(caf->src_field && caf->src_field->data) {
				memset(&dst_data, 0x20, 280);
				cob_move(caf->src_field, &dst);
				memcpy(ib->dtf_value, (&dst)->data, (&dst)->size);
			}
			else {
				if(caf->size > sizeof(ib->dtf_value)) copy_size = sizeof(ib->dtf_value);
				else copy_size = caf->size;


				if(caf->src_field && !caf->src_field->data && !caf->flags.occurs_flag) {
					field_data = get_parent_base_data(caf);
					if(field_data) {
						memcpy(ib->dtf_value, field_data, copy_size);
					}
					else {
						memcpy(ib->dtf_value, "Not found", (&empty_anim_field)->size);
					}
				}
				else if(caf->base_field && !caf->flags.occurs_flag) {
					memcpy(ib->dtf_value, caf->base_field, copy_size);
				}
				else if(caf->base_field && caf->flags.occurs_flag && subscript_shift >= 0) {
					memcpy(ib->dtf_value, caf->base_field + subscript_shift, copy_size);
				}
				else {
					memcpy(ib->dtf_value, "Not found", (&empty_anim_field)->size);
				}
			}
			free(f_name_cpy);
			free(caf_field_name_cpy);
			free(subscript);
			return;
		}

		free(caf_field_name_cpy);
		caf = (anim_field*) caf->previous;
	}

	 /* Search local-storage variables */
	while (cafl && cafl->field_name && orig_program_id) {
		/* ignore case differences */
		cafl_field_name_cpy = cob_strdup(cafl->field_name);
		all_to_upper(cafl_field_name_cpy);

		if(strncmp(cafl_field_name_cpy, f_name_cpy, strlen(cafl_field_name_cpy)) == 0 && strncmp(cafl->program_id, orig_program_id, strlen(cafl->program_id)) == 0) {
			if(subscript) subscript_shift = calculate_subscript_shift(subscript, cafl);

			sprintf(work, "%02d", cafl->usage);
			memcpy(ib->dtf_usage, work, 2);
			sprintf(work, "%03d", cafl->size);
			memcpy(ib->dtf_length, work, 3);
			memset(ib->dtf_value, 0x20, 280);
			if(cafl->src_field && cafl->src_field->data) {
				memset(&dst_data, 0x20, 280);
				cob_move(cafl->src_field, &dst);
				memcpy(ib->dtf_value, (&dst)->data, (&dst)->size);
			}
			else {
				if(cafl->size > sizeof(ib->dtf_value)) copy_size = sizeof(ib->dtf_value);
				else copy_size = cafl->size;

				if(cafl->base_field && !cafl->flags.occurs_flag) {
					memcpy(ib->dtf_value, cafl->base_field, copy_size);
				}
				else if(cafl->base_field && cafl->flags.occurs_flag && subscript_shift > 0) {
					memcpy(ib->dtf_value, cafl->base_field + subscript_shift, copy_size);
				}
				else {
					memcpy(ib->dtf_value, "Not found", (&empty_anim_field)->size);
				}				
			}
			free(f_name_cpy);
			free(cafl_field_name_cpy);
			free(subscript);
			return;
		}

		free(cafl_field_name_cpy);
		cafl = (anim_field*) cafl->previous;
	}


	 /* Search working-storage variables */
	while (cafg && cafg->field_name && orig_program_id) {
		/* ignore case differences */
		cafg_field_name_cpy = cob_strdup(cafg->field_name);
		all_to_upper(cafg_field_name_cpy);
		
		if(strncmp(cafg_field_name_cpy, f_name_cpy, strlen(cafg_field_name_cpy)) == 0 && strncmp(cafg->program_id, orig_program_id, strlen(cafg->program_id)) == 0) {
			if(subscript) subscript_shift = calculate_subscript_shift(subscript, cafg);
			
			sprintf(work, "%02d", cafg->usage);
			memcpy(ib->dtf_usage, work, 2);
			sprintf(work, "%03d", cafg->size);
			memcpy(ib->dtf_length, work, 3);
			memset(ib->dtf_value, 0x20, 280);
			if(cafg->src_field && cafg->src_field->data) {
				memset(&dst_data, 0x20, 280);
				cob_move(cafg->src_field, &dst);
				memcpy(ib->dtf_value, (&dst)->data, (&dst)->size);
			}
			else {
				if(cafg->size > sizeof(ib->dtf_value)) copy_size = sizeof(ib->dtf_value);
				else copy_size = cafg->size;

				if(cafg->base_field && !cafg->flags.occurs_flag) {
					memcpy(ib->dtf_value, cafg->base_field, copy_size);
				}
				else if(cafg->base_field && cafg->flags.occurs_flag && subscript_shift >= 0) {
					memcpy(ib->dtf_value, cafg->base_field + subscript_shift, copy_size);
				}
				else {
					memcpy(ib->dtf_value, "Not found", (&empty_anim_field)->size);
				}
			}
			free(f_name_cpy);
			free(cafg_field_name_cpy);
			free(subscript);
			return;
		}

		free(cafg_field_name_cpy);
		cafg = (anim_field*) cafg->previous;
	}


	memcpy(ib->dtf_value, "unused", 6);
	free(f_name_cpy);
	//if(caf_field_name_cpy) free(caf_field_name_cpy);
	//if(cafl_field_name_cpy) free(cafl_field_name_cpy);
	//if(cafg_field_name_cpy) free(cafg_field_name_cpy);
}

int string_contains(char* str1, char* str2) {
	char *str1_dup, *str2_dup;
	int return_value = 0;

	str1_dup = cob_strdup(str1);
	str2_dup = cob_strdup(str2);

	rtrim(str1_dup);
	rtrim(str2_dup);

	if(strstr(str1_dup, str2_dup) != NULL) 
			return_value = 1;

	cob_free(str1_dup);
	cob_free(str2_dup);

	return return_value;
}