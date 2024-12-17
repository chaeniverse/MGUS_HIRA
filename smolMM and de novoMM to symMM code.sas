
libname aa '/vol/userdata13/sta_room417' ;

/**************************/
/**** smm cohort 제작 *****/
/**************************/

/* n수 세기 */
proc sort data= aa.t200_2023q4_18 nodupkey out=jid_num; by jid; run; *48,362;

/* 진선샘 코호트 만드느라 exclusion criteria 진선샘 버전으로 업데이트 되어있음. n수 안맞을수 있음. */
/* D472 없는 애들 뽑는다.*/
proc sql;
create table aa.no_d472_smm as
select *
from aa.t200_2023q4_18 
where jid not in (select distinct jid
						from aa.t200_2023q4_18
						where substr(main_sick,1,4) in ('D472'));
quit; *22,831,043;
/* n수 세기 */
proc sort data=aa.no_d472_smm nodupkey out=no_d472_smm_id; by jid; run; *38,416;


/************************************************/
/*************** Exclusion criteria ***************/
/************************************************/

/*** 1-1. C90 있으면 출력 ***/
proc sql;
create table aa.c90_smm as
select *
from aa.no_d472_smm
where substr(main_sick,1,3) in ('C90');
quit; *1,970,453;
/* n수 세기 */
proc sort data=aa.c90_smm nodupkey out=c90_smm_id; by jid; run; *28,634;


/*** 1-2. C90 한번 받은 애들 뽑기 ***/
proc sql;
create table c90_smm_once as
select distinct jid
from aa.c90_smm
group by jid
having count(jid) =1;
quit; *4,070;

/*** 1-3. C90 두번 이상이면 출력 (= C90 한번 받은 애들 빼기)***/
proc sql;
create table aa.c90_smm_twice as
select *
from aa.c90_smm
where jid not in (select distinct jid
				from aa.c90_smm
				group by jid
				having count(jid) = 1);
quit; *1,966,383;
/* n수 세기 */
proc sort data=aa.c90_smm_twice nodupkey out=c90_smm_twice_id; by jid; run; *24,564;


/*** 2-1. c90 첫 진단 시점 정의 ***/
proc sql;
create table aa.wash_smm as
select *, min(recu_fr_dd) as first_c90_date
from aa.c90_smm_twice
group by jid;
quit; *1,966,383;

/* first_c90_date을 date format으로 정의 */
data aa.wash_smm; set aa.wash_smm;
first_c90_date_tmp = input(first_c90_date, yymmdd10.);
format first_c90_date_tmp yymmdd10.;
drop first_c90_date;
rename first_c90_date_tmp = first_c90_date;
run;


/*** 2-2. first_c90_date가 2007, 2008인 경우의 n수 출력***/
proc sql;
create table diagnosis0708 as
select distinct jid
from aa.wash_smm
where year(first_c90_date) in (2007, 2008);
quit; *3,656;

/*** 2-3. wash out period (first_c90_date가 2007, 2008이면 삭제)***/
proc sql;
create table aa.wash_smm2 as
select *
from aa.wash_smm
where jid not in (select distinct jid
						from aa.wash_smm
						where year(first_c90_date) in (2007, 2008));
quit; *1,675,965;
/* n수 세기 */
proc sort data=aa.wash_smm2 nodupkey out=wash_smm2_id; by jid; run; *20,908;

/*** 3-1. V193 있는 애들 뽑는다. ***/
proc sql;
create table aa.v193_smm as
select *
from aa.wash_smm2
where substr(prcl_sym_tp_cd,1,4) in ('V193');
quit; *1,609,678;
/* n수 세기 */
proc sort data=aa.v193_smm nodupkey out=v193_smm_id; by jid; run; *18,916;

/* n수 체크 겸 & 검토 겸 V193 없는 n수를 센다. */
proc sql;
create table no_v193_smm as
select distinct jid
from aa.wash_smm2
where jid not in (select distinct jid 
				from aa.wash_smm2
				where substr(prcl_sym_tp_cd,1,4) = 'V193');
quit; *1,992;


/*** 4-1. no-V193까지 뺀 이 시점에서 c90 첫 진단시 나이를 정의한다. ***/
/* c90 첫 진단 시 나이나 min(age)나 같은 말이다. c90만 있는 데이터에서 min(age)를 뽑은 거기 때문이다.*/
proc sql;
create table aa.age19_smm as
select *, min(pat_age) as first_c90_age
from aa.v193_smm
group by jid;
quit; *1,609,678;

/*** 4-2. first_c90_age가 <19인 수를 센다. ***/
proc sql;
create table under19_smm as
select distinct jid
from aa.age19_smm
where first_c90_age <19;
quit; *11;

/*** 4-3. first_c90_age가 <19이면 제외한다. ***/
proc sql;
create table aa.age19_smm2 as
select *
from aa.age19_smm
where jid not in (select distinct jid
						from aa.age19_smm
						where first_c90_age <19);
quit; *1,608,927;
/* n수 세기 */
proc sort data=aa.age19_smm2 nodupkey out=aa.age19_smm2_id; by jid; run; *18,905;


/****************************** T530과 T300과 join한다. ******************************/
/************************ 2. SMM ************************/

/* 이미 돌려져 있는 db이기에 run하지 않는다.  */
/* T530 + T200 */
proc sql;
create table aa.T530_T200 as
select *
from aa.T530_2023Q4_18 as a
left join aa.T200_2023Q4_18 as b
on a.mid=b.mid; 
quit;
/* 명세서 결합키로 join한다. */

/* 이미 돌려져 있는 db이기에 run하지 않는다.  */
data aa.T530_T200; set aa.T530_T200;
drug_date = mdy(substr(RECU_FR_DD,5,2), substr(RECU_FR_DD,7,2), substr(RECU_FR_DD,1,4)); format drug_date yymmdd8.;
drug_age = pat_age; /* (240417) first_mm_age 정의를 위해 코드 추가 */
run; 
/* 약물복용일자를 정의한다. */

/* 이미 돌려져 있는 db이기에 run하지 않는다.  */
/*T530_T200 약물있는 것만 뽑기 추가 0122 */
data aa.T530_T200_mmall; set aa.T530_T200;
where div_cd in ('189901ATB', '463301BIJ', '463302BIJ', '463303BIJ', '485701ACH', '485702ACH',
				   '588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB', 
				   '588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
				   '588207ACH', '588207ATB', '667101BIJ', '667102BIJ', '139001ATB', '149401BIJ', '149402BIJ',
					'149404BIJ', '149405BIJ', '149430BIJ', '149432BIJ', '149433BIJ', '149434BIJ', '149403BIJ',
					'149406BIJ', '149431BIJ', '149435BIJ', '248001BIJ', '248002BIJ', '248003BIJ', '248030BIJ',
					'248031BIJ', '248032BIJ', '647801BIJ', '647801BIJ', '647802BIJ', '647802BIJ', '628001ACH',
					'628002ACH', '628003ACH', '628004ACH', '134501BIJ', '134502BIJ', '134503BIJ', '134530BIJ',
					'134531BIJ', '134532BIJ', '134533BIJ', '134534BIJ', '157101BIJ',  '157102ACH', '157103BIJ',
					'157104ACH',  '157104BIJ',  '157105BIJ', '157106BIJ', '157107BIJ', '157108BIJ', '157130BIJ',
					'157131BIJ', '157132BIJ', '157133BIJ', '157134BIJ', '157135BIJ', '157136BIJ', '48110ATB',
					'500200ATB', '420732BIJ', '228301ATB', '228303ATB', '228305ATB', '228306ALQ', '228307ALQ',
					'502201ATB', '502202ATB', '502203ATB', '502204ATB', '111501ATB', '111502ATB', '111503ATB', '111504ATB',
					'527301ATB', '527302ATB', '628201ATB', '628202ATB', '674301ATB', '480304ATB', '480330BIJ',
					'191501ATB', '191502ATB', '191502ATR', '191503ATB', '191503ATR', '191504ATB', '191504ATR', '191505ATB', '191505ATR',
					'470901ATB', '470902ATB', '470903ATB', '216601ATB', '216602ATB', '216603ATB', '216604ATB', '519300ACH',
					'442302ATB', '442302ATE', '511200ATB', '518400ATB',
					'454001ATB', '454001ATD', '454002ATB', '454002ATD', '454003ATB', '454003ATD', '454005ATB',
					'227801ATB', '227801ATR', '227802ATB', '227806ATB') ; 

keep mid jid div_cd drug_date drug_age;
run;

/* 이미 돌려져 있는 db이기에 run하지 않는다.  */
/* T300에서 약물있는 것만 뽑기 */
data T300_mm; set aa.T300_2023Q4_18;
where div_cd in ('189901ATB', '463301BIJ', '463302BIJ', '463303BIJ', '485701ACH', '485702ACH',
				   '588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB', 
				   '588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
				   '588207ACH', '588207ATB', '667101BIJ', '667102BIJ', '139001ATB', '149401BIJ', '149402BIJ',
					'149404BIJ', '149405BIJ', '149430BIJ', '149432BIJ', '149433BIJ', '149434BIJ', '149403BIJ',
					'149406BIJ', '149431BIJ', '149435BIJ', '248001BIJ', '248002BIJ', '248003BIJ', '248030BIJ',
					'248031BIJ', '248032BIJ', '647801BIJ', '647801BIJ', '647802BIJ', '647802BIJ', '628001ACH',
					'628002ACH', '628003ACH', '628004ACH', '134501BIJ', '134502BIJ', '134503BIJ', '134530BIJ',
					'134531BIJ', '134532BIJ', '134533BIJ', '134534BIJ', '157101BIJ',  '157102ACH', '157103BIJ',
					'157104ACH',  '157104BIJ',  '157105BIJ', '157106BIJ', '157107BIJ', '157108BIJ', '157130BIJ',
					'157131BIJ', '157132BIJ', '157133BIJ', '157134BIJ', '157135BIJ', '157136BIJ', '48110ATB',
					'500200ATB', '420732BIJ', '228301ATB', '228303ATB', '228305ATB', '228306ALQ', '228307ALQ',
					'502201ATB', '502202ATB', '502203ATB', '502204ATB', '111501ATB', '111502ATB', '111503ATB', '111504ATB',
					'527301ATB', '527302ATB', '628201ATB', '628202ATB', '674301ATB', '480304ATB', '480330BIJ',
					'191501ATB', '191502ATB', '191502ATR', '191503ATB', '191503ATR', '191504ATB', '191504ATR', '191505ATB', '191505ATR',
					'470901ATB', '470902ATB', '470903ATB', '216601ATB', '216602ATB', '216603ATB', '216604ATB', '519300ACH',
					'442302ATB', '442302ATE', '511200ATB', '518400ATB',
					'454001ATB', '454001ATD', '454002ATB', '454002ATD', '454003ATB', '454003ATD', '454005ATB',
					'227801ATB', '227801ATR', '227802ATB', '227806ATB') ;

run;*2,288,432;
/* t30에서 필요한 약물들만 뽑으신 듯. */

/* 이미 돌려져 있는 db이기에 run하지 않는다.  */
/* T300_mm에 T200을 붙인다. */
proc sql;
create table aa.T300_T200_mm as
select *
from T300_mm as a
left join aa.T200_2023Q4_18 as b
on a.mid=b.mid; 
quit;
/* t30 raw table에서 약물 뽑은 것에 t20 raw table를 명세서 결합키로 조인하였다. */

data aa.T300_T200_mm; set aa.T300_T200_mm;
drug_date = mdy(substr(RECU_FR_DD,5,2), substr(RECU_FR_DD,7,2), substr(RECU_FR_DD,1,4)); format drug_date yymmdd8.;
drug_age = pat_age; /* (240417) first_mm_age 정의를 위해 코드 추가 */
keep mid jid div_cd drug_date drug_age;
run;
/* 마찬가지로 drug_date를 정의하였다. */


/* T530 mm + T300 mm*/
/* 여기 조금 수정하였다. t530_t200_mm -> t530_t200_mmall */
proc sql;
create table aa.T530_T300_mm_2 as
select * 
	from aa.T530_T200_mmall
		union all
select *
	from aa.T300_T200_mm ;
quit; 
/* t530_t200과 t300_t200을 union all로 붙인다. */

/* jid와 drug_date 순으로 정렬 */
proc sort data=aa.T530_T300_mm_2; by jid drug_date; run;

/************************************************************************/
/*** t530_t300에서 screening jid 해당하는 애들만 뽑기 ***/
proc sql;
create table aa.smm_mm as
select jid, drug_date, div_cd, drug_age
from aa.t530_t300_mm_2
where jid in (select distinct jid from aa.age19_smm2_id); quit; *1,688,020;

/* 1-1. mm 약물 정의 (mm 약물 있으면 mm_yn=1로 없으면 mm_yn=0으로 정의) */
data aa.smm_mm2; set aa.smm_mm;
if (div_cd in ('189901ATB', '463301BIJ', '463302BIJ', '463303BIJ', '485701ACH', '485702ACH',
									'588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB',
									'588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
									'588207ACH', '588207ATB'
)) then mm_yn = 1; run;
/* 1-2. mm_yn=. 인 애들은 mm_yn=0으로 해준다. */
data aa.smm_mm2; set aa.smm_mm2;
if mm_yn=. then mm_yn=0; run;

/* 1-3. 첫 mm 복용일을 정의한다. */
proc sql;
create table first_mm as
select jid, min(drug_date) as first_mm_date, min(drug_age) as first_mm_age
from aa.smm_mm2
where mm_yn=1
group by jid;
quit;
data first_mm; set first_mm;
format first_mm_date yymmdd8.; run; 

/* 1-4. 약물 고유키 출력 */
proc sort data=aa.smm_mm2; by jid descending mm_yn; run;
proc sort data=aa.smm_mm2 nodupkey out=aa.smm_mm_id; by jid; run;

/* 1-5. 약물 고유키 db에 first_mm_date, first_mm_age를 붙인다. */
proc sql;
create table aa.smm_mm3 as
select a.jid, a.mm_yn, b.first_mm_date, b.first_mm_age
from aa.smm_mm_id as a left join first_mm as b on a.jid=b.jid; quit;
/* 이따가 screenig db에 다 갖다 붙일거다. */

/* 2. t20에서 마지막 진단일을 정의한다. */
proc sql;
create table last_dig_date as
select jid, max(recu_to_dd) as last_dig_date
from aa.t200_2023q4_18
group by jid; quit;
/* last_dig_date를 date format으로 정의 */
data last_dig_date; set last_dig_date;
last_dig_date_tmp = input(last_dig_date, yymmdd10.);
format last_dig_date_tmp yymmdd10.;
drop last_dig_date;
rename last_dig_date_tmp = last_dig_date; run;
/* 이따가 screenig db에 갖다 붙일거다. */

/*** 3. 사망 정의 ***/
/* 3-1. dgrslt_tp_cd_2로 정의(dgrslt_tp_cd='4'가 한번이라도 있을 경우 dgrslt_tp_cd_2=1로 본다.) */
data dgrslt_tp_cd_2; set aa.t200_2023q4_18;
if dgrslt_tp_cd='4' then dgrslt_tp_cd_2=1; 
keep jid dgrslt_tp_cd dgrslt_tp_cd_2; run;
/* 3-2. dgrslt_tp_cd_2=. 인 애들은 dgrslt_tp_cd_2=0으로 해준다. */
data dgrslt_tp_cd_2; set dgrslt_tp_cd_2;
if dgrslt_tp_cd_2=. then dgrslt_tp_cd_2=0; run;

/* 3-3. dgrslt_tp_cd_2 고유키 출력 */
proc sort data=dgrslt_tp_cd_2; by jid descending dgrslt_tp_cd_2; run;
proc sort data=dgrslt_tp_cd_2 nodupkey out=dgrslt_tp_cd_2_id; by jid; run;
/* 이따가 screenig db에 갖다 붙일거다. */

/* 여기서 다 갖다 붙이기 */
proc sql;
create table aa.smm_cohort as
select a.jid, a.sex_tp_cd, a.first_c90_date, a.first_c90_age,  
b.mm_yn, b.first_mm_date, b.first_mm_age,  
c.last_dig_date,  d.dgrslt_tp_cd_2
from aa.age19_smm2_id as a left join aa.smm_mm3 as b on a.jid=b.jid
left join last_dig_date as c on a.jid=c.jid
left join dgrslt_tp_cd_2_id as d on a.jid=d.jid; quit; *18,905;

/* 3-4. 최종 사망 정의 */
data aa.smm_cohort2; set aa.smm_cohort;
if last_dig_date < mdy(11,30,2021) or dgrslt_tp_cd_2 = 1 then death_yn=1; run;
/* death_yn=. 인 애들은 death_yn=1으로 해준다. */
data aa.smm_cohort2; set aa.smm_cohort2;
if death_yn=. then death_yn=0; run;

/* death_date 정의 */
data aa.smm_cohort2; set aa.smm_cohort2;
if death_yn=1 then death_date = last_dig_date;
format death_date yymmdd8.; run;

/* death_yn 세기 */
proc freq data=aa.smm_cohort2; table death_yn; run; *death_yn=1  n=10,849;

/* death 관련 변수 정의 */
data aa.smm_cohort3; set aa.smm_cohort2;
if death_yn=0 then death_day = mdy(11,30,2022) - first_mm_date;
else if death_yn=1 then death_day = death_date - first_mm_date;
death_year = death_day/365.65;
run;

/***************************************************************/
/************************ outcome 정의 ************************/
/***************************************************************/

/* 1-1. +-60일 이내 & 혹은 그보다 더 이전을 세어본다. */
proc sql;
create table total_60 as
select *,
case when (first_mm_date - first_c90_date) <= 60 then 1 end as total_60
from aa.smm_cohort3
where mm_yn=1; quit;
proc freq data=total_60; table total_60; run; *total_60=1  n=13,655;


/* 1-2. first c90 date 이후 6개월 이내 사망 */
proc sql;
create table within_6mths as
select *,
case when (death_date - first_c90_date) <=180 then 1 end as within_6mths
from aa.smm_cohort3
where death_date >= first_c90_date and death_yn=1;
run;
proc freq data=within_6mths; table within_6mths; run; *within_6mths=1  n=2,934;


proc sql;
create table aa.smm_cohort4 as
select a.*, b.total_60, c.within_6mths
from aa.smm_cohort3 as a left join total_60 as b on a.jid=b.jid
left join within_6mths as c on a.jid=c.jid;
quit;

/* only_6mths 생성 */
data aa.smm_cohort4; set aa.smm_cohort4;
if total_60 = . and within_6mths = 1 then only_6mths=1; run; 
proc freq data=aa.smm_cohort4; table only_6mths; run; *only_6mths=1  1,143;



/* 1-5. C90 이후 <=60일 처방이면 mm_outcome=1로 정의한다. */
data aa.smm_cohort5; set aa.smm_cohort4;
if total_60 = 1 then mm_outcome=1;
run;
proc freq data= aa.smm_cohort5; table mm_outcome; run; *mm_outcome=1  n=13,655;

/* death within 6mths 빼기 */
data aa.smm_cohort5; set aa.smm_cohort5;
if only_6mths=.; run; *17,762;


/* mm_outcome=.이면 smm=1로 정의 */
data aa.smm_cohort5; set aa.smm_cohort5;
if mm_outcome=. then smm=1; run;
proc freq data=aa.smm_cohort5; table smm; run; *smm=1  4,107;

/* smm=1인 data만 select */
data smm; set aa.smm_cohort5;
if smm=1; 
drop mm_outcome; run;

/* aa.smm_cohort5에서 mm_outcome=1만 keep */
data aa.smm_cohort5; set aa.smm_cohort5;
if mm_outcome=1; run; *13,655;


/* smm db에서 mm_yn=1이면 mm_outcome=1로 코딩 */
data smm; set smm;
if mm_yn=1 then mm_outcome=1; run;
proc freq data=smm; table mm_outcome; run; *mm_outcome=1  1,884;

/* aa.smm_cohort5와 smm을 merge */
data aa.final_smm_cohort; 
set aa.smm_cohort5 smm; run;

/* mm_outcome=.이면 0을 부여 */
data aa.final_smm_cohort; set aa.final_smm_cohort; 
if mm_outcome = . then mm_outcome=0; run;
proc freq data=aa.final_smm_cohort; table mm_outcome; run; *mm_outcome=1  15,539;



/************************************************/
/********************* CCI part ******************/
/************************************************/

/* aa.final_smm_cohort에 해당하는 애들 데려온다. */
proc sql;
create table smm_cci as
select jid, main_sick, sub_sick, recu_fr_dd
from aa.t200_2023q4_18
where jid in (select jid from aa.final_smm_cohort); quit; *;
data smm_cci; set smm_cci;
dig_date = mdy(substr(recu_fr_dd,5,2), substr(recu_fr_dd,7,2), substr(recu_fr_dd,1,4)); format dig_date yymmdd10.; run;

/* aa.final_smm_cohort에서 first_mm_date를 가져와 붙인다. */
proc sql;
create table smm_cci2 as
select a.*, b.first_mm_date
from smm_cci as a left join aa.final_smm_cohort as b on a.jid=b.jid; quit;


/*** CCI 작업 ***/
proc sql;
create table smm_cci3 as
select jid, 
	/*MI*/
	max(case when (substr(main_sick,1,3) in ('I21','I22') or  substr(main_sick,1,4)='I252') and first_mm_date >= dig_date then 1 else 0 end) as mi_yn1,
	sum(case when (substr(sub_sick,1,3) in ('I21','I22') or  substr(sub_sick,1,4)='I252') and first_mm_date >= dig_date then 1 else 0 end) as mi_yn0, 
	/*CHF*/
	max(case when (substr(main_sick,1,3) in ('I43','I50') or  substr(main_sick,1,4) in ('I099','I110','I130','I132','I255','I420','I425','I426','I427','I428','I429','P290')) and first_mm_date >= dig_date then 1 else 0 end) as chf_yn1,
    sum(case when (substr(sub_sick,1,3) in ('I43','I50') or  substr(sub_sick,1,4) in ('I099','I110','I130','I132','I255','I420','I425','I426','I427','I428','I429','P290')) and first_mm_date >= dig_date then 1 else 0 end) as chf_yn0,
    /*PVD*/
    max(case when (substr(main_sick,1,3) in ('I70','I71') or  substr(main_sick,1,4) in ('I731','I738','I739','I771','I790','I792','K551','K558','K559','Z958','Z959')) and first_mm_date >= dig_date then 1 else 0 end) as pvd_yn1,
    sum(case when (substr(sub_sick,1,3) in ('I70','I71') or  substr(sub_sick,1,4) in ('I731','I738','I739','I771','I790','I792','K551','K558','K559','Z958','Z959')) and first_mm_date >= dig_date then 1 else 0 end) as pvd_yn0,
    /*CVD*/
    max(case when (substr(main_sick,1,3) in ('G45','G46','I60','I61','I62','I63','I64','I65','I66','I67','I68','I69') or  substr(main_sick,1,4) ='H340') and first_mm_date >= dig_date then 1 else 0 end) as cvd_yn1,
    sum(case when (substr(sub_sick,1,3) in ('G45','G46','I60','I61','I62','I63','I64','I65','I66','I67','I68','I69') or  substr(sub_sick,1,4) ='H340') and first_mm_date >= dig_date then 1 else 0 end) as cvd_yn0,
    /*Dementia*/
    max(case when (substr(main_sick,1,3) in ('F00','F01','F02','F03','G30') or  substr(main_sick,1,4) in ('F051','G311')) and first_mm_date >= dig_date then 1 else 0 end) as dem_yn1,
    sum(case when (substr(sub_sick,1,3) in ('F00','F01','F02','F03','G30') or  substr(sub_sick,1,4) in ('F051','G311')) and first_mm_date >= dig_date then 1 else 0 end) as dem_yn0,
    /*CPD*/
	max(case when (substr(main_sick,1,3) in ('J40','J41','J42','J43','J44','J45','J46','J47','J60','J61','J62','J63','J64','J65','J66','J67') or  substr(main_sick,1,4) in ('I278','I279','J684','J701','J703')) and first_mm_date >= dig_date then 1 else 0 end) as cpd_yn1,
    sum(case when (substr(sub_sick,1,3) in ('J40','J41','J42','J43','J44','J45','J46','J47','J60','J61','J62','J63','J64','J65','J66','J67') or  substr(sub_sick,1,4) in ('I278','I279','J684','J701','J703')) and first_mm_date >= dig_date then 1 else 0 end) as cpd_yn0,
    /*Rheumatic disease*/
    max(case when (substr(main_sick,1,3) in ('M05','M06','M32','M33','M34') or  substr(main_sick,1,4) in ('M315','M351','M353','M360')) and first_mm_date >= dig_date then 1 else 0 end) as rhe_yn1,
    sum(case when (substr(sub_sick,1,3) in ('M05','M06','M32','M33','M34') or  substr(sub_sick,1,4) in ('M315','M351','M353','M360')) and first_mm_date >= dig_date then 1 else 0 end) as rhe_yn0,
    /*PUD*/
    max(case when (substr(main_sick,1,3) in ('K25','K26','K27','K28')) and first_mm_date >= dig_date then 1 else 0 end) as pud_yn1,
    sum(case when (substr(sub_sick,1,3) in ('K25','K26','K27','K28')) and first_mm_date >= dig_date then 1 else 0 end) as pud_yn0,
    /*MLD*/
    max(case when (substr(main_sick,1,3) in ('B18','K73','K74') or  substr(main_sick,1,4) in ('K700','K701','K702','K703','K709','K713','K714','K715','K717','K760','K762','K763','K764','K768','K769','Z944')) and first_mm_date >= dig_date then 1 else 0 end) as mld_yn1,
    sum(case when (substr(sub_sick,1,3) in ('B18','K73','K74') or  substr(sub_sick,1,4) in ('K700','K701','K702','K703','K709','K713','K714','K715','K717','K760','K762','K763','K764','K768','K769','Z944')) and first_mm_date >= dig_date then 1 else 0 end) as mld_yn0,
    /*DWOC*/
	max(case when (substr(main_sick,1,4) in ('E100','E101','E106','E108','E109','E110','E111','E116','E118','E119','E120','E121','E126','E128','E129','E130','E131','E136','E138','E139','E140','E141','E146','E148','E149')) and first_mm_date >= dig_date then 1 else 0 end) as dwoc_yn1,
    sum(case when (substr(sub_sick,1,4) in ('E100','E101','E106','E108','E109','E110','E111','E116','E118','E119','E120','E121','E126','E128','E129','E130','E131','E136','E138','E139','E140','E141','E146','E148','E149')) and first_mm_date >= dig_date then 1 else 0 end) as dwoc_yn0,
    /*DWCC*/
	max(case when (substr(main_sick,1,4) in ('E102','E103','E104','E105','E107','E112','E113','E114','E115','E117','E122','E123','E124','E125','E127','E132','E133','E134','E135','E137','E142','E143','E144','E145','E147')) and first_mm_date >= dig_date then 1 else 0 end) as dwcc_yn1,
    sum(case when (substr(sub_sick,1,4) in ('E102','E103','E104','E105','E107','E112','E113','E114','E115','E117','E122','E123','E124','E125','E127','E132','E133','E134','E135','E137','E142','E143','E144','E145','E147')) and first_mm_date >= dig_date then 1 else 0 end) as dwcc_yn0,
    /*Hemiplegia or paraplegia*/
    max(case when (substr(main_sick,1,3) in ('G81','G82') or  substr(main_sick,1,4) in ('G041','G114','G801','G802','G830','G831','G832','G833','G834','G839')) and first_mm_date >= dig_date then 1 else 0 end) as hp_yn1,
    sum(case when (substr(sub_sick,1,3) in ('G81','G82') or  substr(sub_sick,1,4) in ('G041','G114','G801','G802','G830','G831','G832','G833','G834','G839')) and first_mm_date >= dig_date then 1 else 0 end) as hp_yn0,
   /*Renal diseases*/
    max(case when (substr(main_sick,1,3) in ('N18','N19') or  substr(main_sick,1,4) in ('I120','I131','N032','N033','N034','N035','N036','N037','N052','N053','N054','N055','N056','N057','N250','Z490','Z491','Z492','Z940','Z992')) and first_mm_date >= dig_date then 1 else 0 end) as rd_yn1,
    sum(case when (substr(sub_sick,1,3) in ('N18','N19') or  substr(sub_sick,1,4) in ('I120','I131','N032','N033','N034','N035','N036','N037','N052','N053','N054','N055','N056','N057','N250','Z490','Z491','Z492','Z940','Z992')) and first_mm_date >= dig_date then 1 else 0 end) as rd_yn0,
  /*Any cancer*/
   max(case when (substr(main_sick,1,3) in ('C00','C01','C02','C03','C04','C05','C06','C07','C08','C09',
									'C10','C11','C12','C13','C14','C15','C16','C17','C18','C19',
									'C20','C21','C22','C23','C24','C25','C26','C30','C31','C32',
									'C33','C34','C37','C38','C39','C40','C41','C43','C44','C45',
									'C46','C47','C48','C49','C50','C51','C52','C53','C54','C55',
									'C56','C57','C58','C60','C61','C62','C63','C64','C65','C66',
									'C67','C68','C69','C70','C71','C72','C73','C74','C75','C76',
									'C81','C82','C83','C84','C85',
									'C88','C91','C92','C93','C94','C95','C96','C97','C77','C78','C79','C80')) and first_mm_date >= dig_date then 1 else 0 end) as cancer_yn1,
    sum(case when (substr(sub_sick,1,3) in ('C00','C01','C02','C03','C04','C05','C06','C07','C08','C09',
									'C10','C11','C12','C13','C14','C15','C16','C17','C18','C19',
									'C20','C21','C22','C23','C24','C25','C26','C30','C31','C32',
									'C33','C34','C37','C38','C39','C40','C41','C43','C44','C45',
									'C46','C47','C48','C49','C50','C51','C52','C53','C54','C55',
									'C56','C57','C58','C60','C61','C62','C63','C64','C65','C66',
									'C67','C68','C69','C70','C71','C72','C73','C74','C75','C76',
									'C81','C82','C83','C84','C85',
									'C88','C91','C92','C93','C94','C95','C96','C97','C77','C78','C79','C80')) and first_mm_date >= dig_date then 1 else 0 end) as cancer_yn0,
    /*Moderate and severe liver disease*/
    max(case when (substr(main_sick,1,4) in ('I850','I859','I864','I982','K704','K711','K721','K729','K765','K766','K767')) and first_mm_date >= dig_date then 1 else 0 end) as sld_yn1,
    sum(case when (substr(sub_sick,1,4) in ('I850','I859','I864','I982','K704','K711','K721','K729','K765','K766','K767')) and first_mm_date >= dig_date then 1 else 0 end) as sld_yn0,
   /*Metastatic solid tumor*/
    max(case when (substr(main_sick,1,3) in ('C77','C78','C79','C80')) and first_mm_date >= dig_date then 1 else 0 end) as mst_yn1,
    sum(case when (substr(sub_sick,1,3) in ('C77','C78','C79','C80')) and first_mm_date >= dig_date then 1 else 0 end) as mst_yn0,
   /*AIDS/HIV*/
    max(case when (substr(main_sick,1,3) in ('B20','B21','B22','B24')) and first_mm_date >= dig_date then 1 else 0 end) as aids_yn1,
    sum(case when (substr(sub_sick,1,3) in ('B20','B21','B22','B24')) and first_mm_date >= dig_date then 1 else 0 end) as aids_yn0

 from smm_cci2 
 group by jid;
 quit;



data smm_cci_yes; set smm_cci3;
	if mi_yn1=1 or mi_yn0 >=2 then mi_yes=1; else mi_yes=0;
	if chf_yn1=1 or chf_yn0 >=2 then chf_yes=1; else chf_yes=0;
	if pvd_yn1=1 or pvd_yn0 >=2 then pvd_yes=1; else pvd_yes=0;
	if cvd_yn1=1 or cvd_yn0 >=2 then cvd_yes=1; else cvd_yes=0;
	if dem_yn1=1 or dem_yn0 >=2 then dem_yes=1; else dem_yes=0; 
	if cpd_yn1=1 or cpd_yn0>=2 then cpd_yes=1; else cpd_yes=0;
	if rhe_yn1=1 or rhe_yn0>=2 then rhe_yes=1; else rhe_yes=0;
	if pud_yn1=1 or pud_yn0>=2 then pud_yes=1; else pud_yes=0;
	if mld_yn1=1 or mld_yn0>=2 then mld_yes=1; else mld_yes=0;
	if dwoc_yn1=1 or dwoc_yn0>=2 then dwoc_yes=1; else dwoc_yes=0;
	if dwcc_yn1=1 or dwcc_yn0>=2 then dwcc_yes=1; else dwcc_yes=0; 
	if hp_yn1=1 or hp_yn0>=2 then hp_yes=1; else hp_yes=0;
	if rd_yn1=1 or rd_yn0>=2 then rd_yes=1; else rd_yes=0;
	if cancer_yn1=1 or cancer_yn0>=2 then cancer_yes=1; else cancer_yes=0;
	if sld_yn1=1 or sld_yn0>=2 then sld_yes=1; else sld_yes=0;
	if mst_yn1=1 or mst_yn0>=2 then mst_yes=1; else mst_yes=0;
	if aids_yn1=1 or aids_yn0>=2 then aids_yes=1; else aids_yes=0; 
keep jid mi_yes chf_yes pvd_yes cvd_yes dem_yes cpd_yes rhe_yes pud_yes mld_yes dwoc_yes dwcc_yes 
		hp_yes rd_yes cancer_yes sld_yes mst_yes aids_yes;
run;

/* smm에서 최종 코호트? 만들기 */
proc sql;
create table aa.smm_surv as
select *
from aa.final_smm_cohort as a left join smm_cci_yes as b on a.jid=b.jid;
quit;



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
