      *>  Copyright (C) 2017  Free Software Foundation, Inc.
      *>  Written by Brian Tiffin, from information in OpenJDK 8 jni.h
      *>
      *>  This file is part of GnuCOBOL.
      *>
      *>  The GnuCOBOL compiler is free software: you can redistribute
      *>  it and/or modify it under the terms of the GNU General Public
      *>  License as published by the Free Software Foundation, either
      *>  version 3 of the License, or (at your option) any later
      *>  version.
      *>
      *>  GnuCOBOL is distributed in the hope that it will be useful,
      *>  but WITHOUT ANY WARRANTY; without even the implied warranty of
      *>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      *>  GNU General Public License for more details.
      *>
      *>  You should have received a copy of the GNU General Public
      *>  License along with GnuCOBOL.
      *>  If not, see <http://www.gnu.org/licenses/>.

       01 JVM-ENV usage pointer EXTERNAL AS "JVM_ENV".
       01 JVM-ENVIRONMENT EXTERNAL AS "JVM_ENVIRONMENT".
          05 filler                    usage pointer.   
          05 filler                    usage pointer.   
          05 filler                    usage pointer.   
          05 filler                    usage pointer.   

          05 GetVersion                usage program-pointer.   
          05 DefineClass               usage program-pointer.   
          05 FindClass                 usage program-pointer.
          05 filler occurs 8 times.
             10 filler                 usage program-pointer.
          05 ExceptionOccurred         usage program-pointer.   
          05 ExceptionDescribe         usage program-pointer.   
          05 ExceptionClear            usage program-pointer.   
          05 filler occurs 95 times.
             10 filler                 usage program-pointer.
          05 GetStaticMethodID         usage program-pointer.   
          05 CallStaticObjectMethod    usage program-pointer.   
          05 filler occurs 14 times.
             10 filler                 usage program-pointer.
          05 CallStaticIntMethod       usage program-pointer.   
          05 filler occurs 11 times.
             10 filler                 usage program-pointer.
          05 CallStaticVoidMethod      usage program-pointer.   
          05 filler occurs 25 times.
             10 filler                 usage program-pointer.
          05 NewStringUTF              usage program-pointer.
          05 GetStringUTFLength        usage program-pointer.
          05 GetStringUTFChars         usage program-pointer.
          05 ReleaseStringUTFChars     usage program-pointer.
          05 GetArrayLength            usage program-pointer.
          05 NewObjectArray            usage program-pointer.   
          05 GetObjectArrayElement     usage program-pointer.   
          05 SetObjectArrayElement     usage program-pointer.   
          05 filler occurs 53 times.
             10 filler                 usage program-pointer.
          05 ExceptionCheck            usage program-pointer.
          05 filler occurs 4 times.
             10 filler                 usage program-pointer.
