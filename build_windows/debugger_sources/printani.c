/* Generated by            cobc 2.0.20140120 */
/* Generated from          printani.cob */
/* Generated at            Nov 20 2014 13:38:40 */
/* GNU Cobol build date    Nov 20 2014 13:37:03 */
/* GNU Cobol package date  Jan 20 2014 07:40:53 UTC */
/* Compile command         ..\cobc -C printani.cob */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <math.h>
#define  COB_KEYWORD_INLINE __inline
#include <libcob.h>

#define  COB_SOURCE_FILE		"printani.cob"
#define  COB_PACKAGE_VERSION		"2.0"
#define  COB_PATCH_LEVEL		20140120
#define  COB_MODULE_FORMATTED_DATE	"Nov 20 2014 13:38:40"
#define  COB_MODULE_DATE		20141120
#define  COB_MODULE_TIME		133840

/* Global variables */
#include "printani.c.h"

/* Function prototypes */

__declspec(dllexport) int			printani (cob_u8_t *, cob_u8_t *, cob_u8_t *);
static int		printani_ (const int, cob_u8_t *, cob_u8_t *, cob_u8_t *);

/* Functions */

/* PROGRAM-ID 'printani' */

/* ENTRY 'printani' */

int
printani (cob_u8_t *b_21, cob_u8_t *b_23, cob_u8_t *b_22)
{
  return printani_ (0, b_21, b_23, b_22);
}

static int
printani_ (const int entry, cob_u8_t *b_21, cob_u8_t *b_23, 
	cob_u8_t *b_22)
{
  /* Program local variables */
  #include "printani.c.l.h"

  /* Start of function code */

  /* CANCEL callback */
  if (unlikely(entry < 0)) {
  	goto P_cancel;
  }

  /* Check initialized, check module allocated, */
  /* set global pointer, */
  /* push module stack, save call parameter count */
  cob_module_enter (&module, &cob_glob_ptr, 0);

  /* Set address of module parameter list */
  module->cob_procedure_params = cob_procedure_params;

  /* Set frame stack pointer */
  frame_ptr = frame_stack;
  frame_ptr->perform_through = 0;

  /* Initialize program */
  if (unlikely(initialized == 0)) {
  	goto P_initialize;
  }
  P_ret_initialize:

  /* Increment module active */
  module->module_active++;

  /* Save number of call params */
  module->module_num_params = cob_glob_ptr->cob_call_params;

  /* Entry dispatch */
  goto l_2;

  /* PROCEDURE DIVISION */

  /* Line: 47        : Entry     printani                : printani.cob */
  l_2:;

  /* Line: 47        : OPEN               : printani.cob */
  cob_open (h_ANI, 1, 0, &f_19);
  if (unlikely(cob_glob_ptr->cob_exception_code != 0))
  {
    /* PERFORM Default Error Handler */
    frame_ptr++;
    frame_ptr->perform_through = 1;
    frame_ptr->return_address_num = 0;
    goto l_1;
    l_6:
    frame_ptr--;
  }

  /* Line: 52        : MOVE               : printani.cob */
  memcpy (b_20, "00000001", 8);

  /* Line: 54        : READ               : printani.cob */
  cob_read_next (h_ANI, &f_19, 1);
  if (unlikely(cob_glob_ptr->cob_exception_code != 0))
  {
    /* PERFORM Default Error Handler */
    frame_ptr++;
    frame_ptr->perform_through = 1;
    frame_ptr->return_address_num = 1;
    goto l_1;
    l_7:
    frame_ptr--;
  }
  else
  {
    cob_move (&f_17, &f_18);
  }

  /* Line: 56        : PERFORM            : printani.cob */
  for (;;)
  {

    /* Line: 57        : IF                 : printani.cob */
    if (((int)cob_cmp_numdisp (b_20, 8, (*(int *)(b_22)), 0) > 0))
    {

      /* Line: 62        : EXIT PERFORM       : printani.cob */
      goto l_5;
    }

    /* Line: 65        : MOVE               : printani.cob */
    memset (b_18, 32, 255);

    /* Line: 66        : READ               : printani.cob */
    cob_read_next (h_ANI, &f_19, 1);
    if (unlikely(cob_glob_ptr->cob_exception_code != 0))
    {
      if (cob_glob_ptr->cob_exception_code == 0x0501)
      {

        /* Line: 67        : EXIT PERFORM       : printani.cob */
        goto l_5;
      }
      else
      {
        /* PERFORM Default Error Handler */
        frame_ptr++;
        frame_ptr->perform_through = 1;
        frame_ptr->return_address_num = 2;
        goto l_1;
        l_8:
        frame_ptr--;
      }
    }
    else
    {
      cob_move (&f_17, &f_18);
    }

    /* Line: 71        : STRING             : printani.cob */
    cob_string_init (COB_SET_FLD(f0, 256, b_23 + 256 * (cob_get_numdisp (b_20, 8) - 1), &a_2), NULL);
    cob_string_delimited (NULL);
    cob_string_append (&f_18);
    cob_string_finish ();

    /* Line: 75        : ADD                : printani.cob */
    cob_add_int (&f_20, 1, 0);
  }

  /* Implicit EXIT label */
  l_5:;

  /* Line: 81        : CLOSE              : printani.cob */
  cob_close (h_ANI, &f_19, 0, 0);
  if (unlikely(cob_glob_ptr->cob_exception_code != 0))
  {
    /* PERFORM Default Error Handler */
    frame_ptr++;
    frame_ptr->perform_through = 1;
    frame_ptr->return_address_num = 3;
    goto l_1;
    l_9:
    frame_ptr--;
  }

  /* Line: 83        : GOBACK             : printani.cob */
  goto exit_program;

  /* Program exit */

  exit_program:

  /* Decrement module active count */
  if (module->module_active) {
  	module->module_active--;
  }

  /* Pop module stack */
  cob_module_leave (module);

  /* Program return */
  return b_1;


  /* Paragraph Default Error Handler   */
  l_1:;

  if (!(cob_glob_ptr->cob_error_file->flag_select_features & COB_SELECT_FILE_STATUS)) {
  	cob_fatal_error (COB_FERROR_FILE);
  }

  /* Implicit Default Error Handler return */
  if (frame_ptr->perform_through == 1)
    goto P_switch;

  /* Fatal error if reached */
  cob_fatal_error (COB_FERROR_CODEGEN);


  /* Frame stack jump table */
  P_switch:
   switch (frame_ptr->return_address_num) {
   case 3:
     goto l_9;
   case 2:
     goto l_8;
   case 1:
     goto l_7;
   case 0:
     goto l_6;
   }
   cob_fatal_error (COB_FERROR_CODEGEN);


  /* Program initialization */
  P_initialize:

  cob_check_version (COB_SOURCE_FILE, COB_PACKAGE_VERSION, COB_PATCH_LEVEL);

  /* Initialize module structure */
  module->module_name = "printani";
  module->module_formatted_date = COB_MODULE_FORMATTED_DATE;
  module->module_source = COB_SOURCE_FILE;
  module->module_entry.funcptr = (void *(*)())printani;
  module->module_cancel.funcptr = (void *(*)())printani_;
  module->collating_sequence = NULL;
  module->crt_status = NULL;
  module->cursor_pos = NULL;
  module->module_ref_count = NULL;
  module->module_path = &cob_module_path;
  module->module_active = 0;
  module->module_date = COB_MODULE_DATE;
  module->module_time = COB_MODULE_TIME;
  module->module_type = 0;
  module->module_param_cnt = 3;
  module->module_returning = 0;
  module->ebcdic_sign = 0;
  module->decimal_point = '.';
  module->currency_symbol = '$';
  module->numeric_separator = ',';
  module->flag_filename_mapping = 1;
  module->flag_binary_truncate = 1;
  module->flag_pretty_display = 1;
  module->flag_host_sign = 0;
  module->flag_no_phys_canc = 0;
  module->flag_main = 0;
  module->flag_fold_call = 0;
  module->flag_exit_program = 0;

  /* Initialize cancel callback */
  cob_set_cancel (module);

  /* Initialize WORKING-STORAGE */
  b_1 = 0;
  memset (b_18, 32, 255);
  memset (b_19, 48, 2);
  memset (b_20, 48, 8);


  if (!h_ANI)
  {
    h_ANI = cob_cache_malloc (sizeof(cob_file));
  }
  if (!k_ANI)
  {
    k_ANI = cob_cache_malloc (sizeof (cob_file_key) * 1);
  }
  k_ANI->field = &f_6;
  k_ANI->flag = 0;
  k_ANI->offset = 0;
  h_ANI->select_name = (const char *)"ani";
  h_ANI->file_status = h_ANI_status;
  memset (h_ANI_status, '0', 2);
  h_ANI->assign = COB_SET_DATA (f_21, b_21);
  h_ANI->record = &f_17;
  h_ANI->variable_record = NULL;
  h_ANI->record_min = 74;
  h_ANI->record_max = 74;
  h_ANI->nkeys = 1;
  h_ANI->keys = k_ANI;
  h_ANI->file = NULL;
  h_ANI->fd = -1;
  h_ANI->organization = 3;
  h_ANI->access_mode = 2;
  h_ANI->lock_mode = 0;
  h_ANI->open_mode = 0;
  h_ANI->flag_optional = 0;
  h_ANI->last_open_mode = 0;
  h_ANI->flag_operation = 0;
  h_ANI->flag_nonexistent = 0;
  h_ANI->flag_end_of_file = 0;
  h_ANI->flag_begin_of_file = 0;
  h_ANI->flag_first_read = 0;
  h_ANI->flag_read_done = 0;
  h_ANI->flag_select_features = 1;
  h_ANI->flag_needs_nl = 0;
  h_ANI->flag_needs_top = 0;
  h_ANI->file_version = 1;

  initialized = 1;
  goto P_ret_initialize;

  /* CANCEL callback handling */
  P_cancel:

  if (!initialized) {
  	return 0;
  }
  if (module->module_active) {
  	cob_fatal_error (COB_FERROR_CANCEL);
  }

  cob_close (h_ANI, NULL, COB_CLOSE_NORMAL, 1);
  cob_cache_free (k_ANI);
  k_ANI = NULL;
  cob_cache_free (h_ANI);
  h_ANI = NULL;
  b_1 = 0;
  cob_cache_free (module);
  module = NULL;

  initialized = 0;
  return 0;

}

/* End PROGRAM-ID 'printani' */

/* End functions */

