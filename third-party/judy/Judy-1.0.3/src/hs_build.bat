@echo off

rem WARMING!!! Don't Edit This File!!! Pugs' Build System Depends On This!

echo --- This is a compile kit to suggest how to port to your machine
echo --- This script runs in 7 seconds on a 3.2Ghz Pentium P4C
echo --- Must be in the 'src' directory to execute this script
echo --- I normally run it like: COPT='-O3 -march=i686' sh_build

rem echo --- Set Compiler
rem set CC=D:\ghc\ghc-6.4.2\gcc.exe

rem echo --- Set Optimization

rem COPT='-O'
rem set COPT=-ID:\ghc\ghc-6.4.2\include\mingw -ID:\ghc\ghc-6.4.2\gcc-lib\include -BD:\ghc\ghc-6.4.2\gcc-lib

rem echo --- Set Shared library option
rem set CPIC=-fPIC
set CPIC=

set CP_F=perl -MExtUtils::Command -e cp
set RM_F=perl -MExtUtils::Command -e rm_f

echo --- Compile JudyMalloc - common to Judy1 and JudyL
rem echo cd JudyCommon
cd JudyCommon
%RM_F% *.o
%CC%  %COPT% %CPIC% -I. -I.. -c JudyMalloc.c
rem echo cd ..
cd ..

echo --- Give Judy1 the proper names
rem echo cd Judy1
cd Judy1
%RM_F% *.o
%CP_F% ..\JudyCommon\JudyByCount.c      	Judy1ByCount.c
%CP_F% ..\JudyCommon\JudyCascade.c      	Judy1Cascade.c
%CP_F% ..\JudyCommon\JudyCount.c        	Judy1Count.c
%CP_F% ..\JudyCommon\JudyCreateBranch.c 	Judy1CreateBranch.c
%CP_F% ..\JudyCommon\JudyDecascade.c    	Judy1Decascade.c
%CP_F% ..\JudyCommon\JudyDel.c          	Judy1Unset.c
%CP_F% ..\JudyCommon\JudyFirst.c        	Judy1First.c
%CP_F% ..\JudyCommon\JudyFreeArray.c    	Judy1FreeArray.c
%CP_F% ..\JudyCommon\JudyGet.c          	Judy1Test.c
%CP_F% ..\JudyCommon\JudyGet.c          	j__udy1Test.c
%CP_F% ..\JudyCommon\JudyInsArray.c     	Judy1SetArray.c
%CP_F% ..\JudyCommon\JudyIns.c          	Judy1Set.c
%CP_F% ..\JudyCommon\JudyInsertBranch.c 	Judy1InsertBranch.c
%CP_F% ..\JudyCommon\JudyMallocIF.c     	Judy1MallocIF.c
%CP_F% ..\JudyCommon\JudyMemActive.c    	Judy1MemActive.c
%CP_F% ..\JudyCommon\JudyMemUsed.c      	Judy1MemUsed.c
%CP_F% ..\JudyCommon\JudyPrevNext.c     	Judy1Next.c
%CP_F% ..\JudyCommon\JudyPrevNext.c     	Judy1Prev.c
%CP_F% ..\JudyCommon\JudyPrevNextEmpty.c	Judy1NextEmpty.c
%CP_F% ..\JudyCommon\JudyPrevNextEmpty.c	Judy1PrevEmpty.c
%CP_F% ..\JudyCommon\JudyTables.c	        Judy1TablesGen.c


echo --- This table is constructed from Judy1.h data to match malloc(3) needs
rem echo %CC% %COPT%  -I. -I.. -I..\JudyCommon -DJUDY1 Judy1TablesGen.c -o Judy1TablesGen
%CC% %COPT%  -I. -I.. -I..\JudyCommon -DJUDY1 Judy1TablesGen.c -o Judy1TablesGen
%RM_F% *.o
.\Judy1TablesGen
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Tables.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Tables.c

echo --- Compile the main line Judy1 modules
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Test.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Test.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYGETINLINE j__udy1Test.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYGETINLINE j__udy1Test.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Set.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Set.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1SetArray.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1SetArray.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Unset.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Unset.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1First.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1First.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYNEXT Judy1Next.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYNEXT Judy1Next.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYPREV Judy1Prev.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYPREV Judy1Prev.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYNEXT Judy1NextEmpty.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYNEXT Judy1NextEmpty.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYPREV Judy1PrevEmpty.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DJUDYPREV Judy1PrevEmpty.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Count.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Count.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DNOSMARTJBB -DNOSMARTJBU -DNOSMARTJLB Judy1ByCount.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 -DNOSMARTJBB -DNOSMARTJBU -DNOSMARTJLB Judy1ByCount.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1FreeArray.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1FreeArray.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1MemUsed.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1MemUsed.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1MemActive.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1MemActive.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Cascade.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Cascade.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Decascade.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1Decascade.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1CreateBranch.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1CreateBranch.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1InsertBranch.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1InsertBranch.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1MallocIF.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDY1 Judy1MallocIF.c
rem echo cd ..
cd ..

echo --- Give JudyL the proper names
rem echo cd JudyL
cd JudyL
%RM_F% *.o
%CP_F% ..\JudyCommon\JudyByCount.c      	JudyLByCount.c
%CP_F% ..\JudyCommon\JudyCascade.c              JudyLCascade.c
%CP_F% ..\JudyCommon\JudyCount.c        	JudyLCount.c
%CP_F% ..\JudyCommon\JudyCreateBranch.c 	JudyLCreateBranch.c
%CP_F% ..\JudyCommon\JudyDecascade.c    	JudyLDecascade.c
%CP_F% ..\JudyCommon\JudyDel.c          	JudyLDel.c
%CP_F% ..\JudyCommon\JudyFirst.c        	JudyLFirst.c
%CP_F% ..\JudyCommon\JudyFreeArray.c    	JudyLFreeArray.c
%CP_F% ..\JudyCommon\JudyGet.c          	JudyLGet.c
%CP_F% ..\JudyCommon\JudyGet.c          	j__udyLGet.c
%CP_F% ..\JudyCommon\JudyInsArray.c     	JudyLInsArray.c
%CP_F% ..\JudyCommon\JudyIns.c          	JudyLIns.c
%CP_F% ..\JudyCommon\JudyInsertBranch.c 	JudyLInsertBranch.c
%CP_F% ..\JudyCommon\JudyMallocIF.c     	JudyLMallocIF.c
%CP_F% ..\JudyCommon\JudyMemActive.c    	JudyLMemActive.c
%CP_F% ..\JudyCommon\JudyMemUsed.c      	JudyLMemUsed.c
%CP_F% ..\JudyCommon\JudyPrevNext.c     	JudyLNext.c
%CP_F% ..\JudyCommon\JudyPrevNext.c     	JudyLPrev.c
%CP_F% ..\JudyCommon\JudyPrevNextEmpty.c	JudyLNextEmpty.c
%CP_F% ..\JudyCommon\JudyPrevNextEmpty.c	JudyLPrevEmpty.c
%CP_F% ..\JudyCommon\JudyTables.c	        JudyLTablesGen.c

echo --- This table is constructed from JudyL.h data to match malloc(3) needs
rem echo %CC% %COPT%  -I. -I.. -I..\JudyCommon -DJUDYL JudyLTablesGen.c -o JudyLTablesGen
%CC% %COPT%  -I. -I.. -I..\JudyCommon -DJUDYL JudyLTablesGen.c -o JudyLTablesGen
%RM_F% *.o
.\JudyLTablesGen
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLTables.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLTables.c

echo --- Compile the main line JudyL modules
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLGet.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLGet.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYGETINLINE j__udyLGet.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYGETINLINE j__udyLGet.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLIns.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLIns.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLIns.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLInsArray.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLDel.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLDel.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLFirst.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLFirst.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYNEXT JudyLNext.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYNEXT JudyLNext.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYPREV JudyLPrev.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYPREV JudyLPrev.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYNEXT JudyLNextEmpty.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYNEXT JudyLNextEmpty.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYPREV JudyLPrevEmpty.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DJUDYPREV JudyLPrevEmpty.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLCount.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLCount.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DNOSMARTJBB -DNOSMARTJBU -DNOSMARTJLB JudyLByCount.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL -DNOSMARTJBB -DNOSMARTJBU -DNOSMARTJLB JudyLByCount.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLFreeArray.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLFreeArray.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLMemUsed.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLMemUsed.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLMemActive.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLMemActive.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLCascade.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLCascade.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLDecascade.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLDecascade.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLCreateBranch.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLCreateBranch.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLInsertBranch.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLInsertBranch.c
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLMallocIF.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c -DJUDYL JudyLMallocIF.c
rem echo cd ..
cd ..

echo --- Compile the JudySL routine
rem echo cd JudySL
cd JudySL
%RM_F% *.o
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c JudySL.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c JudySL.c
rem echo cd ..
cd ..
echo --- Compile the JudyHS routine
rem echo cd JudyHS
cd JudyHS
%RM_F% *.o
rem echo %CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c JudyHS.c
%CC%  %COPT% %CPIC% -I. -I.. -I..\JudyCommon -c JudyHS.c
rem echo cd ..
cd ..

rem Make a Judy shared library with CPIC='-fPIC' above
rem ld -shared -o libJudy.so Judy*\*.o
rem
rem  -OR-
rem
echo --- Make a Judy static library
%AR_CALL%
D:\ghc\ghc-6.4.2\bin\ar -r libJudy.a Judy1/j__udy1Test.o Judy1/Judy1ByCount.o Judy1/Judy1Cascade.o Judy1/Judy1Count.o Judy1/Judy1CreateBranch.o Judy1/Judy1Decascade.o Judy1/Judy1First.o Judy1/Judy1FreeArray.o Judy1/Judy1InsertBranch.o Judy1/Judy1MallocIF.o Judy1/Judy1MemActive.o Judy1/Judy1MemUsed.o Judy1/Judy1Next.o Judy1/Judy1NextEmpty.o Judy1/Judy1Prev.o Judy1/Judy1PrevEmpty.o Judy1/Judy1Set.o Judy1/Judy1SetArray.o Judy1/Judy1Tables.o Judy1/Judy1Test.o Judy1/Judy1Unset.o JudyCommon/JudyMalloc.o JudyHS/JudyHS.o JudyL/j__udyLGet.o JudyL/JudyLByCount.o JudyL/JudyLCascade.o JudyL/JudyLCount.o JudyL/JudyLCreateBranch.o JudyL/JudyLDecascade.o JudyL/JudyLDel.o JudyL/JudyLFirst.o JudyL/JudyLFreeArray.o JudyL/JudyLGet.o JudyL/JudyLIns.o JudyL/JudyLInsArray.o JudyL/JudyLInsertBranch.o JudyL/JudyLMallocIF.o JudyL/JudyLMemActive.o JudyL/JudyLMemUsed.o JudyL/JudyLNext.o JudyL/JudyLNextEmpty.o JudyL/JudyLPrev.o JudyL/JudyLPrevEmpty.o JudyL/JudyLTables.o JudySL/JudySL.o

echo --- Done
