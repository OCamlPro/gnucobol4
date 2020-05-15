GnuCOBOL 3.1 Release Notes
==========================

[snip]
(The following section explains this file.  It should be removed prior
to producing a release candidate tarball.)

Developer Notes
---------------

These Release-Notes describe 

1. Changes incorporated since the last release
2. Known issues in this release
3. problem reports, by number, that were resolved

Initially, this file captured the open issues in the tracking system
for `branches/gnucobol-3.x`. The intention is that every open issue be
"resolved" in some sense for the release, whether closed as invalid,
fixed, or deferred for a future release.

Each issue was assigned a status and anticipated action that represent
a proposed resolution. As a developer, you should feel free to move
anything under Known Issues to Resolved Issues, whether because you
fixed them, or determined it was invalid. Feature requests (Wish List
items) can be moved between the 3.2 and 4.0 sections, too.

Please do not move anything into "Include in 3.1" without consensus
unless you're promising it is fixed or will by 15 June 2020. If you
think something *must* be in the 3.1 release, but you yourself can't
commit to doing the work, that's what the mailing lists and discussion
boards are for.

[pins]

Changes to GnuCOBOL since version 2.2
=====================================

It has been many moons since the last release.  To be precise,
approximately 23 moons. Much has happened in that time.

For quite a few years (not just moons), work on GnuCOBOL had proceeded
along separate, independent, incompatible branches.  That left
different developers free to work on areas that interested them, but
didn't serve the user community very well. That changed in 2019, with
a huge merge under the code name "pangaea". After resolving a host of
(anticipated) conflicts, that work was then merged into the mainline
"trunk", from which this release was subsequently derived.

GnuCOBOL, being a work of man, is not perfect, and may never be
"done". However, we are proud and happy to report, there is
progress. The testsuite covers more territory, and more tests
pass. The REPORTWRITER feature is now incorporated. Output can take
the form of XML or JSON. And many, many bugs were discovered and
fixed.

Below are some of the highlights: things to look for, to beware of,
and to look forward too.

Changes to `cobc` compiler
--------------------------

### New Features

### Breaking Changes

Changes to `libcob` runtime library
-----------------------------------

### New Features

### Breaking Changes


Known Issues
============

Below are known issues that we hoped would be fixed in this release,
but were not.  For full details, and for other pending issues that
were never deemed candidates for this release, see
the
[bug-tracking system](https://sourceforge.net/p/open-cobol/_list/tickets).

Include in 3.1
--------------

* [577](https://sourceforge.net/p/open-cobol/bugs/577/) Simon Sobisch
  $ in col7 - code line ignored
  Status: [Fix](https://sourceforge.net/p/open-cobol/bugs/577/#e32c/d651/a313)
          supplied in patch
  Action: Apply patch.  Defer to 4.0 if tests fail. 
		  

Plan for 3.2
------------

(none yet)


Defered to 4.0
--------------

###User Interface

* [453](https://sourceforge.net/p/open-cobol/bugs/453/) Brian Tiffin
  cobc -h -v
  Status: verbose ignored if **-v** appears afer **-h** on the command line. 

* [529](https://sourceforge.net/p/open-cobol/bugs/529/) James K. Lowden
  configuration directory can't be modified [patch]
  Status: on 
          [Wish List #342](https://sourceforge.net/p/open-cobol/feature-requests/342/)
  Action: Notation in release notes

* [635](https://sourceforge.net/p/open-cobol/bugs/635/)
  cobc bug and proc div header AND -W | -Wall
  Status: opinion
  Action: close & ignore

* [637](https://sourceforge.net/p/open-cobol/bugs/637/)
  cobc odity and mis-definition over -W  and -Wall
  Status: duplicates #635
  Action: close & ignore

###Generated Code

* [50](https://sourceforge.net/p/open-cobol/bugs/50/) Simon Sobisch
  Line sequential record length wrong
  Status: [Bug](https://sourceforge.net/p/open-cobol/bugs/50/#227d)
          In this release, Cobol programs fail to correctly detect
          variable record lengths in line-sequential files.

* [142](https://sourceforge.net/p/open-cobol/bugs/142/)
  FUNCTION ORD, FUNCTION CHAR not honouring COLLATING SEQUENCE alphabet
  Status: In this release, the ORD and CHAR functions do not honor
          COLLATING SEQUENCE.  No discussion. 

* [211](https://sourceforge.net/p/open-cobol/bugs/211/)
  using parameters default
  Status: [Bug](https://sourceforge.net/p/open-cobol/bugs/211/#0f91)
          Mutiple issues, cf. 
		  [bugs:#67](https://sourceforge.net/p/open-cobol/bugs/67/) and 
		  [bugs:#109](https://sourceforge.net/p/open-cobol/bugs/109/)
  Action: Sort out and document for release. 

* [512](https://sourceforge.net/p/open-cobol/bugs/512/)
  Word boundary and SYNC RIGHT.
  Status: Indeterminate.  No discussion. Needs verification. 

* [535](https://sourceforge.net/p/open-cobol/bugs/535/) Simon Sobisch
  VBISAM data record size error on variable length record
  Status: Indeterminate.  Needs verification. 

* [550](https://sourceforge.net/p/open-cobol/bugs/550/)
  abnormal results using DECIMAL-FLOAT-16
  Status: Verified as of 2018-09-02.  Bug report includes test. 
  Action: In this release, computation with DECIMAL-FLOAT-16 produces
          erronious results.

* [578](https://sourceforge.net/p/open-cobol/bugs/578/)
  VB-ISAM 2.0.1 unexpected test results
  Status: Seems valid
  Action: Notation in release notes

* [575](https://sourceforge.net/p/open-cobol/bugs/575/)
  The writer after behaviour over a line sequential file
  Status: `WRITE ... AFTER n` is not reliable in this release. 
  Action: Notation in release notes

* [589](https://sourceforge.net/p/open-cobol/bugs/589/)
  Read next ISAM file record skips records
  Status: [Bug](https://sourceforge.net/p/open-cobol/bugs/589/#f3d2) 
          limited to vb-isam, not the DBD default. 
  Action: Notation in release notes

* [604](https://sourceforge.net/p/open-cobol/bugs/604/)
  Line sequential problem
  Status: [Bug](https://sourceforge.net/p/open-cobol/bugs/604/#1ff1) under Windows
  Action: Notation in release notes

* [613](https://sourceforge.net/p/open-cobol/bugs/613/)
  Another display bug
  Data output in excess of terminal width are sometimes discarded. 
  Status: Seems valid. 
  Action: Notation in release notes

* [620](https://sourceforge.net/p/open-cobol/bugs/620/)
  COBC - Internal cross reference problem
  Status: Indeterminate.
  Action: If truly "internal" function, ignore for now. 

* [594](https://sourceforge.net/p/open-cobol/bugs/594/)
  Run-time error messages are not displayed when extended screenio is used!
  Status: [Indeterminate](https://sourceforge.net/p/open-cobol/bugs/594/#8225)
  Action: Verify with supplied example

* [588](https://sourceforge.net/p/open-cobol/bugs/588/)
  Accept/display bugs
  Status: Unclear
  Action: Ignore pending clarification

* [626](https://sourceforge.net/p/open-cobol/bugs/626/)
  MOVE from redefined field to numeric fails (IBM dialect difference)
  Status: Language Lawyer
  Action: Notation in release notes if required

* [631](https://sourceforge.net/p/open-cobol/bugs/631/)
  Possible uninitialized variable somewhere
  Status: Seems valid, but not reproducible 
  Action: Ignore for this release

###Parser

Issues that require changes to the parser

* [36](https://sourceforge.net/p/open-cobol/bugs/36/) Brian Tiffin
  Copy replacing error in fixed format
  
  [Bug: Requires changes to parser](https://sourceforge.net/p/open-cobol/bugs/36/#efaf)
  Status: In this release the result of `COPY REPLACING` 
  must not extend beyond column 72. 

* [150](https://sourceforge.net/p/open-cobol/bugs/150/)
  COBC Options - problem with fmfcomment
  [Bug: incomplete feature](https://sourceforge.net/p/open-cobol/bugs/150/#5f8e/03c8/1e71/a61b/a375)
  Status: In this release, -fmfcomment does not work as intended. 

* [213](https://sourceforge.net/p/open-cobol/bugs/213/)
  cannot call entry in same program.
  Status: In this release, ENTRY can be used with one of 
  [two restrictions]:(https://sourceforge.net/p/open-cobol/bugs/213/#a6d6)
  1. leave the ENTRY and CALL STATIC "thing", but LINKAGE will need to
     be fixed length, not ANY.
  2. move the ENTRY to a full PROCEDURE.

* [256](https://sourceforge.net/p/open-cobol/bugs/256/) Edward Hart
  class condition against alphabet-name resuluts in &#34;Error: Invalid expression&#34;
  Status: Indeterminate.  Needs verification. 

* [496](https://sourceforge.net/p/open-cobol/bugs/496/)
  cobc -Xref without global seq
  Status: Indeterminate.  No discussion. Needs verification. 

* [590](https://sourceforge.net/p/open-cobol/bugs/590/)
  Incorrect test of array index with -x
  Status: Seems valid

* [607](https://sourceforge.net/p/open-cobol/bugs/607/)
  Reverse allowed instead of Reverse-Video
  Status: Bug. Per report, "REVERSE" is allowed in place of
          "REVERSE-VIDEO", except if using the Function All Intrinsic
          option.
  Action: Notation in release notes

* [633](https://sourceforge.net/p/open-cobol/bugs/633/)
  attempt to reference unallocated memory
  Status: Verified.
          Do to [incorrect use](https://sourceforge.net/p/open-cobol/bugs/633/#15e2)
  Action: Notation in release notes


###UDF

Issues that affect user-defined functions

* [251](https://sourceforge.net/p/open-cobol/bugs/251/)
  BY VALUE {literal} musings
  No discussion.
  Status: In this release, parameters cannot be reliably, uniformly passed to UDFs. 
  Action: Add PR description to the release notes. 

* [112](https://sourceforge.net/p/open-cobol/bugs/112/)
  UDF does not work with numeric literal
  Status: Per #251

* [113](https://sourceforge.net/p/open-cobol/bugs/113/)
  UDF does not work with arithmetic expression
  Status: Per #251

###Performance

* [532](https://sourceforge.net/p/open-cobol/bugs/532/) Simon Sobisch
  Very slow loading BDB ISAM database using non-unique alternate keys 
  Status: No discussion. Seems valid. 
  Action: Warning in release notes. 

Resolved Issues
---------------

[This section tracks any problem reports that 

1. were open when this file was created (Fri 15 May 2020), and
2. were resolved such that they require no future attention. 

This whole section is just an aid to release engineering. It's a
temporary depot for resolved issues, until the release, in case
something comes up to make them "unresolved" again.

Upon release, this section will be removed from the file. It is not
necessary to track an arbitrary set of closed issues here; for that
kind of detail, the user may consult the tracking system.]

