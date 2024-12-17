libname aa '/vol/userdata13/sta_room417' ;



/************************************************************************/
/************ Part 1. 얘는 약제 분포 보기 위함 ***************/
/************************************************************************/


/* R에서 MGUS to MM 가져오기 (N=281) */
proc import out=mgus_to_mm
datafile="/vol/userdata13/sta_room417/mgus_to_mm.csv"
dbms=csv
replace;
getnames=yes;
run; 
data mgus_to_mm; set mgus_to_mm; jid1=compress(put(jid, 20.)); run;
data mgus_to_mm; set mgus_to_mm; drop jid; rename jid1=jid; run;


/*** merge하기 ***/
/* mgus */
data MGUS_to_MM; set mgus_to_mm; group=1; run; *281;
data sMM_to_MM; set aa.final_smm_cohort; if mm_outcome=1; group=2; run; *1,884;
data denovoMM; set aa.denovoMM; group=3; run; *13,655;



%MACRO GROUP(var);
proc sql;
create table &var. as
select jid, first_mm_date, first_mm_age, group
from &var.; quit;
%MEND;
%GROUP(MGUS_to_MM);
%GROUP(sMM_to_MM);
%GROUP(denovoMM);

data all_groups; set MGUS_to_MM sMM_to_MM denovoMM; run; *15,820;


%MACRO MEDI(DBNAME);
proc sql;
create table MM_medication as
select mid, jid, div_cd, drug_date
from aa.t530_t300_mm_2
where jid in (select jid from &DBNAME.); quit; *;

proc sql;
create table first_mm_date as
select jid, first_mm_date, first_mm_age, group
from &DBNAME.; quit; *;

proc sql;
create table MM_medication2 as
select a.*, b.first_mm_date, b.first_mm_age, b.group
from MM_medication as a left join first_mm_date as b on a.jid=b.jid; quit; *;


proc sql;
create table MM_medication3 as
select *
from MM_medication2
where (div_cd in ('189901ATB', '463301BIJ', '463302BIJ', '463303BIJ', '485701ACH', '485702ACH',
									'588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB',
									'588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
									'588207ACH', '588207ATB') and (first_mm_date <= drug_date <= first_mm_date + 60))
group by jid;
quit; *;

/* 약제를 category로 분류 */
proc sql;
create table MM_medication4 as
select *,
case when div_cd in ('189901ATB') then 1 else 0 end as mel_yn,
case when div_cd in ('463301BIJ', '463302BIJ', '463303BIJ') then 1 else 0 end as borte_yn,
case when div_cd in ('485701ACH', '485702ACH') then 1 else 0 end as thali_yn,
case when div_cd in ('588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB',
									'588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
									'588207ACH', '588207ATB') then 1 else 0 end as lenal_yn
from MM_medication3;
quit;

proc sql;
create table MM_medication5 as
select *,
max(case when mel_yn =1 then 1 else 0 end) as mel_yn1,
max(case when borte_yn = 1 then 1 else 0 end) as borte_yn1,
max(case when thali_yn = 1 then 1 else 0 end) as thali_yn1,
max(case when lenal_yn = 1 then 1 else 0 end) as lenal_yn1
from MM_medication4
group by jid;
quit;


data MM_medication6; set MM_medication5; drop mel_yn borte_yn thali_yn lenal_yn; run;
proc sort data=MM_medication6 nodupkey out=MM_medication_id; by jid; run;


proc sql;
create table MM_medication_id as
select *,
/* 단독 사용 */
case when (mel_yn1=1 and borte_yn1=0 and thali_yn1=0 and lenal_yn1=0) then 1 else 0 end as mel_only,
case when (mel_yn1=0 and borte_yn1=1 and thali_yn1=0 and lenal_yn1=0) then 1 else 0 end as borte_only,
case when (mel_yn1=0 and borte_yn1=0 and thali_yn1=1 and lenal_yn1=0) then 1 else 0 end as thali_only,
case when (mel_yn1=0 and borte_yn1=0 and thali_yn1=0 and lenal_yn1=1) then 1 else 0 end as lenal_only,

/* 2개 combination */
case when (mel_yn1=1 and borte_yn1=1 and thali_yn1=0 and lenal_yn1=0) then 1 else 0 end as mel_borte,
case when (mel_yn1=1 and borte_yn1=0 and thali_yn1=1 and lenal_yn1=0) then 1 else 0 end as mel_thali,
case when (mel_yn1=1 and borte_yn1=0 and thali_yn1=0 and lenal_yn1=1) then 1 else 0 end as mel_lenal,

case when (mel_yn1=0 and borte_yn1=1 and thali_yn1=1 and lenal_yn1=0) then 1 else 0 end as borte_thali,
case when (mel_yn1=0 and borte_yn1=1 and thali_yn1=0 and lenal_yn1=1) then 1 else 0 end as borte_lenal,
case when (mel_yn1=0 and borte_yn1=0 and thali_yn1=1 and lenal_yn1=1) then 1 else 0 end as thali_lenal,

/* 3개 combination */
case when (mel_yn1=1 and borte_yn1=1 and thali_yn1=1 and lenal_yn1=0) then 1 else 0 end as mel_borte_thali,
case when (mel_yn1=1 and borte_yn1=1 and thali_yn1=0 and lenal_yn1=1) then 1 else 0 end as mel_borte_lenal,
case when (mel_yn1=0 and borte_yn1=1 and thali_yn1=1 and lenal_yn1=1) then 1 else 0 end as borte_thali_lenal,

/* 4개 combination */
case when (mel_yn1=1 and borte_yn1=1 and thali_yn1=1 and lenal_yn1=1) then 1 else 0 end as mel_borte_thali_lenal

from MM_medication_id; quit;


proc freq data=MM_medication_id; table group*(mel_only borte_only thali_only lenal_only mel_borte mel_thali mel_lenal borte_thali borte_lenal thali_lenal mel_borte_thali mel_borte_lenal borte_thali_lenal mel_borte_thali_lenal)/expected chisq fisher; run;

/* 다시 묶자 */
proc sql;
create table MM_medication_id2 as
select *,
case when mel_only=1 then 1 else 0 end as mp,
case when (borte_only=1 or borte_lenal=1) then 1 else 0 end as vd,
case when (borte_thali=1 or borte_thali_lenal=1) then 1 else 0 end as vtd,
case when (mel_borte=1 or mel_borte_thali=1 or mel_borte_lenal=1) then 1 else 0 end as vmp,
case when (mel_thali=1 or mel_lenal=1) then 1 else 0 end as other
from MM_medication_id; quit;

/*other term 제외*/
proc sql;
create table MM_medication_id3 as
select *
from MM_medication_id2
where jid not in (select jid from MM_medication_id2 where other=1); quit;

proc freq data=MM_medication_id3; table group*(mp vd thali_only lenal_only vmp vtd other)/expected chisq fisher; run;

%MEND;
%MEDI(DBNAME=all_groups);


/************************************************************************************/
/************ Part 2. 얘는 약제 term을 분석에 필요한 db에 붙이는 과정 ***************/
/************************************************************************************/
/* Part 2에서 만든걸 R로 갖고 가서 분석한다. */

/* denovo MM 따로 만들기(13,655) */
proc sql;
create table aa.denovoMM as
select *
from aa.smm_surv
where smm =.; run; *13,655;


/* final sMM cohort 따로 만들기(4,107) */
proc sql;
create table aa.final_smm_cohort as
select *
from aa.smm_surv
where smm=1; run; *4,107;

/* aa.mgus_surv_cci, aa.final_smm_cohort, aa.denovoMM에 medication 있냐 없냐를 변수로 붙인다.*/
data aa.mgus_surv_mm; set aa.mgus_surv_cci; run;

%MACRO MEDI2(DBNAME);
proc sql;
create table MM_medication as
select mid, jid, div_cd, drug_date
from aa.t530_t300_mm_2
where jid in (select jid from aa.&DBNAME.); quit; *;

proc sql;
create table first_mm_date as
select jid, first_mm_date, first_mm_age
from aa.&DBNAME.
where first_mm_date ^= .; quit; *;

proc sql;
create table MM_medication2 as
select a.*, b.first_mm_date, b.first_mm_age
from MM_medication as a left join first_mm_date as b on a.jid=b.jid; quit; *;


proc sql;
create table MM_medication3 as
select *
from MM_medication2
where (div_cd in ('189901ATB', '463301BIJ', '463302BIJ', '463303BIJ', '485701ACH', '485702ACH',
									'588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB',
									'588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
									'588207ACH', '588207ATB') and (first_mm_date <= drug_date <= first_mm_date + 60))
group by jid;
quit; *;

/* 약제를 category로 분류 */
proc sql;
create table MM_medication4 as
select *,
case when div_cd in ('189901ATB') then 1 else 0 end as mel_yn,
case when div_cd in ('463301BIJ', '463302BIJ', '463303BIJ') then 1 else 0 end as borte_yn,
case when div_cd in ('485701ACH', '485702ACH') then 1 else 0 end as thali_yn,
case when div_cd in ('588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB',
									'588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
									'588207ACH', '588207ATB') then 1 else 0 end as lenal_yn
from MM_medication3;
quit;

proc sql;
create table MM_medication5 as
select *,
max(case when mel_yn =1 then 1 else 0 end) as mel_yn1,
max(case when borte_yn = 1 then 1 else 0 end) as borte_yn1,
max(case when thali_yn = 1 then 1 else 0 end) as thali_yn1,
max(case when lenal_yn = 1 then 1 else 0 end) as lenal_yn1
from MM_medication4
group by jid;
quit;



data MM_medication6; set MM_medication5; drop mel_yn borte_yn thali_yn lenal_yn; run;
proc sort data=MM_medication6 nodupkey out=MM_medication_id; by jid; run;


proc sql;
create table MM_medication_id as
select *,
/* 단독 사용 */
case when (mel_yn1=1 and borte_yn1=0 and thali_yn1=0 and lenal_yn1=0) then 1 else 0 end as mel_only,
case when (mel_yn1=0 and borte_yn1=1 and thali_yn1=0 and lenal_yn1=0) then 1 else 0 end as borte_only,
case when (mel_yn1=0 and borte_yn1=0 and thali_yn1=1 and lenal_yn1=0) then 1 else 0 end as thali_only,
case when (mel_yn1=0 and borte_yn1=0 and thali_yn1=0 and lenal_yn1=1) then 1 else 0 end as lenal_only,

/* 2개 combination */
case when (mel_yn1=1 and borte_yn1=1 and thali_yn1=0 and lenal_yn1=0) then 1 else 0 end as mel_borte,
case when (mel_yn1=1 and borte_yn1=0 and thali_yn1=1 and lenal_yn1=0) then 1 else 0 end as mel_thali,
case when (mel_yn1=1 and borte_yn1=0 and thali_yn1=0 and lenal_yn1=1) then 1 else 0 end as mel_lenal,

case when (mel_yn1=0 and borte_yn1=1 and thali_yn1=1 and lenal_yn1=0) then 1 else 0 end as borte_thali,
case when (mel_yn1=0 and borte_yn1=1 and thali_yn1=0 and lenal_yn1=1) then 1 else 0 end as borte_lenal,
case when (mel_yn1=0 and borte_yn1=0 and thali_yn1=1 and lenal_yn1=1) then 1 else 0 end as thali_lenal,

/* 3개 combination */
case when (mel_yn1=1 and borte_yn1=1 and thali_yn1=1 and lenal_yn1=0) then 1 else 0 end as mel_borte_thali,
case when (mel_yn1=1 and borte_yn1=1 and thali_yn1=0 and lenal_yn1=1) then 1 else 0 end as mel_borte_lenal,
case when (mel_yn1=0 and borte_yn1=1 and thali_yn1=1 and lenal_yn1=1) then 1 else 0 end as borte_thali_lenal,

/* 4개 combination */
case when (mel_yn1=1 and borte_yn1=1 and thali_yn1=1 and lenal_yn1=1) then 1 else 0 end as mel_borte_thali_lenal

from MM_medication_id; quit;


/* 다시 묶자 */
proc sql;
create table MM_medication_id2 as
select *,
case when mel_only=1 then 1 else 0 end as mp,
case when (borte_only=1 or borte_lenal=1) then 1 else 0 end as vd,
case when (borte_thali=1 or borte_thali_lenal=1) then 1 else 0 end as vtd,
case when (mel_borte=1 or mel_borte_thali=1 or mel_borte_lenal=1) then 1 else 0 end as vmp,
case when (mel_thali=1 or mel_lenal=1) then 1 else 0 end as other
from MM_medication_id; quit;

proc sql;
create table aa.&dbname. as
select a.*, b.mp, b.vd, b.thali_only, b.lenal_only, b.vmp, b.vtd, b.other
from aa.&dbname. as a left join mm_medication_id2 as b on a.jid=b.jid; quit;
%MEND;
%MEDI2(dbname=mgus_surv_mm);
%MEDI2(dbname=final_smm_cohort);
%MEDI2(dbname=denovoMM);


/************************************************************************************/
/************ Part 3. other에서 chisq, fisher exact test 구하기 ***************/
/************************************************************************************/

/* R에서 MGUS to MM 가져오기 (N=281) */
proc import out=mgus_to_mm
datafile="/vol/userdata13/sta_room417/mgus_to_mm.csv"
dbms=csv
replace;
getnames=yes;
run; 
data mgus_to_mm; set mgus_to_mm; jid1=compress(put(jid, 20.)); run;
data mgus_to_mm; set mgus_to_mm; drop jid; rename jid1=jid; run;

/*** merge하기 ***/
/* mgus */
data MGUS_to_MM; set mgus_to_mm; group=1; run; *281;
data sMM_to_MM; set aa.final_smm_cohort; if mm_outcome=1; group=2; run; *1,884;
data denovoMM; set aa.denovoMM; group=3; run; *13,655;

%MACRO GROUP(var);
proc sql;
create table &var. as
select jid, first_mm_date, first_mm_age, group
from &var.; quit;
%MEND;
%GROUP(MGUS_to_MM);
%GROUP(sMM_to_MM);
%GROUP(denovoMM);


%MACRO MEDI3(DBNAME);
proc sql;
create table MM_medication as
select mid, jid, div_cd, drug_date
from aa.t530_t300_mm_2
where jid in (select jid from &DBNAME.); quit; *;

proc sql;
create table first_mm_date as
select jid, first_mm_date, first_mm_age
from &DBNAME.
where first_mm_date ^= .; quit; *;

proc sql;
create table MM_medication2 as
select a.*, b.first_mm_date, b.first_mm_age
from MM_medication as a left join first_mm_date as b on a.jid=b.jid; quit; *;


proc sql;
create table MM_medication3 as
select *
from MM_medication2
where (div_cd in ('189901ATB', '463301BIJ', '463302BIJ', '463303BIJ', '485701ACH', '485702ACH',
									'588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB',
									'588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
									'588207ACH', '588207ATB') and (first_mm_date <= drug_date <= first_mm_date + 60))
group by jid;
quit; *;

/* 약제를 category로 분류 */
proc sql;
create table MM_medication4 as
select *,
case when div_cd in ('189901ATB') then 1 else 0 end as mel_yn,
case when div_cd in ('463301BIJ', '463302BIJ', '463303BIJ') then 1 else 0 end as borte_yn,
case when div_cd in ('485701ACH', '485702ACH') then 1 else 0 end as thali_yn,
case when div_cd in ('588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB',
									'588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
									'588207ACH', '588207ATB') then 1 else 0 end as lenal_yn
from MM_medication3;
quit;

proc sql;
create table MM_medication5 as
select *,
max(case when mel_yn =1 then 1 else 0 end) as mel_yn1,
max(case when borte_yn = 1 then 1 else 0 end) as borte_yn1,
max(case when thali_yn = 1 then 1 else 0 end) as thali_yn1,
max(case when lenal_yn = 1 then 1 else 0 end) as lenal_yn1
from MM_medication4
group by jid;
quit;



data MM_medication6; set MM_medication5; drop mel_yn borte_yn thali_yn lenal_yn; run;
proc sort data=MM_medication6 nodupkey out=MM_medication_id; by jid; run;


proc sql;
create table MM_medication_id as
select *,
/* 단독 사용 */
case when (mel_yn1=1 and borte_yn1=0 and thali_yn1=0 and lenal_yn1=0) then 1 else 0 end as mel_only,
case when (mel_yn1=0 and borte_yn1=1 and thali_yn1=0 and lenal_yn1=0) then 1 else 0 end as borte_only,
case when (mel_yn1=0 and borte_yn1=0 and thali_yn1=1 and lenal_yn1=0) then 1 else 0 end as thali_only,
case when (mel_yn1=0 and borte_yn1=0 and thali_yn1=0 and lenal_yn1=1) then 1 else 0 end as lenal_only,

/* 2개 combination */
case when (mel_yn1=1 and borte_yn1=1 and thali_yn1=0 and lenal_yn1=0) then 1 else 0 end as mel_borte,
case when (mel_yn1=1 and borte_yn1=0 and thali_yn1=1 and lenal_yn1=0) then 1 else 0 end as mel_thali,
case when (mel_yn1=1 and borte_yn1=0 and thali_yn1=0 and lenal_yn1=1) then 1 else 0 end as mel_lenal,

case when (mel_yn1=0 and borte_yn1=1 and thali_yn1=1 and lenal_yn1=0) then 1 else 0 end as borte_thali,
case when (mel_yn1=0 and borte_yn1=1 and thali_yn1=0 and lenal_yn1=1) then 1 else 0 end as borte_lenal,
case when (mel_yn1=0 and borte_yn1=0 and thali_yn1=1 and lenal_yn1=1) then 1 else 0 end as thali_lenal,

/* 3개 combination */
case when (mel_yn1=1 and borte_yn1=1 and thali_yn1=1 and lenal_yn1=0) then 1 else 0 end as mel_borte_thali,
case when (mel_yn1=1 and borte_yn1=1 and thali_yn1=0 and lenal_yn1=1) then 1 else 0 end as mel_borte_lenal,
case when (mel_yn1=0 and borte_yn1=1 and thali_yn1=1 and lenal_yn1=1) then 1 else 0 end as borte_thali_lenal,

/* 4개 combination */
case when (mel_yn1=1 and borte_yn1=1 and thali_yn1=1 and lenal_yn1=1) then 1 else 0 end as mel_borte_thali_lenal

from MM_medication_id; quit;

proc sql;
create table &dbname._other as
select a.*, b.mel_thali, b.mel_lenal
from &dbname. as a left join MM_medication_id as b on a.jid=b.jid; quit;
%MEND;
%MEDI3(dbname=mgus_to_mm);
%MEDI3(dbname=smm_to_mm);
%MEDI3(dbname=denovoMM);

/* chisq-test, fisher's exact test 뽑기 위함 */
proc sql;
create table mgus as
select mel_thali, mel_lenal, 1 as group
from mgus_to_mm_other; quit;
proc sql;
create table smm as
select mel_thali, mel_lenal, 2 as group
from final_smm_cohort_other; quit;
proc sql;
create table denovo as
select mel_thali, mel_lenal, 3 as group
from denovoMM_other; quit;

data all_cohort; set mgus smm denovo; run;

proc freq data=all_cohort; table group*(mel_thali mel_lenal)/fisher chisq; run;