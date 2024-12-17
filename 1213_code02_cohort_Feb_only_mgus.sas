
libname aa '/vol/userdata13/sta_room417' ;


/************ MGUS Patients*************/
/* 1. D472 주상병 진단*/
proc sql;
create table mgus as
select *, sum(case when (substr(main_sick,1,4)='D472') then 1 else 0 end) as cnt_d472
from aa.T200_2023Q4_18
where substr(main_sick,1,4) = 'D472'
group by jid
order by jid, RECU_FR_DD; 
quit;
proc sort data=mgus; by jid RECU_FR_DD; run; /*n=9,946*/
proc sort data=mgus nodupkey out=mgus_once; by jid; run; /*n=9,946*/ 


/* 2. D472 only once 제외*/
proc freq data= mgus_once; table cnt_d472; run; /*cnt_mgus_main=1 n=1,909*/ 
data mgus_once_ex; set mgus_once;
where cnt_d472 >=2 ; run; /*n=8,037*/ 


/* 3. 2007년, 2008년 diagnosis 제외*/
data mgus_wash; set mgus_once_ex;
if substr(RECU_FR_DD,1,4) in ('2007', '2008') then ex_mgus_wash=1; else ex_mgus_wash=0; run;
proc freq data= mgus_wash; table ex_mgus_wash; run; /*ex_mgus_wash=1 n=258*/ 

data aa.mgus_wash_ex; set mgus_wash;
where ex_mgus_wash=0; 
mgus_index_date = mdy(substr(RECU_FR_DD,5,2), substr(RECU_FR_DD,7,2), substr(RECU_FR_DD,1,4)); format mgus_index_date yymmdd8. ;
drop ex_mgus_wash ; run; /*n=7,779*/ 
/* d472 진단 후 d472 only once 빼고 07,08년 진단년도 뺐다. 이 시점에서의 recu_fr_dd를 mgus_index_date(mgus 진단일)로 본다. */


/* MGUS + 상병내역 */
proc sql;
create table aa.mgus_cohort_dig as
select a.jid, a.mgus_index_date, b.SEX_TP_CD, b.PAT_AGE, b.YID,
		  b.FOM_TP_CD, b.MAIN_SICK, b.SUB_SICK, b.RECU_FR_DD, b.RECU_TO_DD, b.DGRSLT_TP_CD, b.PRCL_SYM_TP_CD
from aa.mgus_wash_ex  as a 
left join aa.t200_2023q4_18 as b
on a.jid=b.jid ;
quit;

/* 4. D472 진단 후 3개월 이내 C90 diagnosis 제외*/
proc sort data= aa.mgus_cohort_dig; by jid RECU_FR_DD; run;
data aa.mgus_cohort_dig; set aa.mgus_cohort_dig;
dig_date = mdy(substr(RECU_FR_DD,5,2), substr(RECU_FR_DD,7,2), substr(RECU_FR_DD,1,4)); format dig_date yymmdd8. ; run;
/* 이 시점에서 dig_date(진단일자)를 정의하셨다. */

proc sql;
create table c90_3months as
select jid, mgus_index_date, dig_date, main_sick, 
		max(case when substr(main_sick,1,3) = 'C90' and (dig_date-mgus_index_date) <= 90 then 1 else 0 end) as c90_3months 
from aa.mgus_cohort_dig
group by jid
order by jid, dig_date;
quit;

proc sort data=c90_3months nodupkey out=c90_3months_id; by jid; run; 
proc freq data=c90_3months_id; table c90_3months; run;/*1910*/ 


data c90_3months_ex; set c90_3months_id;
where c90_3months=0; 
keep jid;
run; *n=5,869;

proc sql;
create table aa.mgus_cohort_final_dig as
select *
from c90_3months_ex as a
left join aa.mgus_cohort_dig as b on a.jid=b.jid;
quit;
/* 여기까지가 d472 진단 후 3개월 이내 c90진단을 screening 한 것이다. */ *n=5,869;

/* T530 + T200 */
proc sql;
create table aa.T530_T200 as
select *
from aa.T530_2023Q4_18 as a
left join aa.T200_2023Q4_18 as b
on a.mid=b.mid; 
quit;
data aa.T530_T200; set aa.T530_T200;
drug_date = mdy(substr(RECU_FR_DD,5,2), substr(RECU_FR_DD,7,2), substr(RECU_FR_DD,1,4)); format drug_date yymmdd8.;
run; 


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

keep mid jid div_cd drug_date;
run;


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

run;


proc sql;
create table aa.T300_T200_mm as
select *
from T300_mm as a
left join aa.T200_2023Q4_18 as b
on a.mid=b.mid; 
quit;
data aa.T300_T200_mm; set aa.T300_T200_mm;
drug_date = mdy(substr(RECU_FR_DD,5,2), substr(RECU_FR_DD,7,2), substr(RECU_FR_DD,1,4)); format drug_date yymmdd8.;
keep mid jid div_cd drug_date;
run;

/* T530 mm + T300 mm*/
proc sql;
create table aa.T530_T300_mm as
select * 
	from aa.T530_T200_mm
		union all
select *
	from aa.T300_T200_mm ;
quit; 
proc sort data=aa.T530_T300_mm; by jid drug_date; run;



/* + 처방 */
proc sql;
create table aa.mgus_dig_drug as
select a.*, b.drug_date, b.DIV_CD 
from aa.mgus_cohort_final_dig  as a 
left join aa.T530_T300_mm as b
on a.jid=b.jid; 
quit;

data aa.mgus_dig_drug; set aa.mgus_dig_drug;
dig_date = mdy(substr(RECU_FR_DD,5,2), substr(RECU_FR_DD,7,2), substr(RECU_FR_DD,1,4)); format dig_date yymmdd8.;
dig_end_date = mdy(substr(RECU_TO_DD,5,2), substr(RECU_TO_DD,7,2), substr(RECU_TO_DD,1,4)); format dig_end_date yymmdd8.;
drop recu_fr_dd; run;

/* + 처방_all
proc sql;
create table aa.mgus_dig_drug as
select a.*, b.drug_date, b.DIV_CD 
from aa.mgus_cohort_dig  as a 
left join aa.T530_T200_mm as b
on a.jid=b.jid; 
quit;

data aa.mgus_dig_drug; set aa.mgus_dig_drug;
dig_date = mdy(substr(RECU_FR_DD,5,2), substr(RECU_FR_DD,7,2), substr(RECU_FR_DD,1,4)); format dig_date yymmdd8.;
drop recu_fr_dd; run;*/




/******************************* outcome 정의 **************************************/
/*****************************************************
1. MGUS
*****************************************************/
proc sql;
create table c90_diag as
select jid, min(dig_date) as first_c90_date
from aa.mgus_dig_drug
where substr(main_sick,1,3) = 'C90' and dig_date > mgus_index_date
group by jid
having count(*)  >=2;

create table mm_medication as
select a.jid, min(a.drug_date) as c90_medi_date , b.first_c90_date
from aa.mgus_dig_drug a
inner join c90_diag b on a.jid=b.jid
where div_cd in ('189901ATB', '463301BIJ', '463302BIJ', '463303BIJ', '485701ACH', '485702ACH',
				   '588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB', 
				   '588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
				   '588207ACH', '588207ATB') and
				   a.drug_date >= b.first_c90_date
group by a.jid
order by jid, c90_medi_date; 
quit;


proc sql;
create table mgus_mm_final as
select a.jid, b.first_c90_date, c.c90_medi_date,
		case when b.jid is not null and c.jid is not null then 1 else 0 end as mm_outcome 
from (select distinct jid from aa.mgus_dig_drug) a 
left join c90_diag b on a.jid = b.jid 
left join mm_medication c on a.jid=c.jid;
quit; 


proc sort data=mgus_mm_final; by jid first_c90_date descending mm_outcome c90_medi_date; run;
proc sort data=mgus_mm_final nodupkey out=mgus_mm_id; by jid; run;
proc freq data=mgus_mm_id; table mm_outcome; run; 

proc sql;
create table mgus_mm_final_total as
select a.jid, b.first_c90_date,
		case when b.jid is not null then 1 else 0 end as total_mm_outcome 
from (select distinct jid from aa.mgus_dig_drug) a 
left join c90_diag b on a.jid = b.jid ;
quit; 
proc freq data=mgus_mm_final_total; table total_mm_outcome; run;



/*MM outcome*/
proc sql;
create table aa.mgus_mm_out as
select *
from mgus_mm_id a
left join aa.mgus_dig_drug b on a.jid=b.jid; 
quit;


/*Total MM outcome*/
proc sql;
create table aa.mgus_totmm_out as
select *
from mgus_mm_final_total a
left join aa.mgus_dig_drug b on a.jid=b.jid;
quit;

proc sort data=aa.mgus_mm_out; by jid; run;
proc sort data=aa.mgus_totmm_out nodupkey out=mgus_totmm_out; by jid; run;
data mgus_totmm_out; set mgus_totmm_out;
keep jid total_mm_outcome; run;


/************* c90_medi_date, (MM) drug 첫 처방일이 end time ***************/
proc sort data=aa.mgus_mm_out nodupkey out=mgus_mm_df(drop=fom_tp_cd main_sick sub_sick  dgrslt_tp_cd); 
by jid;
run;

proc sql; 
create table aa.mgus_outcome_df as
select *
from mgus_mm_df as a
left join mgus_totmm_out as b on a.jid=b.jid; 
quit;


/*mm 진행 day*/
data a; set aa.mgus_outcome_df;
where total_mm_outcome=1;
format first_c90_date yymmdd8.; 
mm_day = first_c90_date-mgus_index_date ;
mm_year = mm_day/365.65; 
run;
proc univariate data=a; var mm_day mm_year; run;

/*mm 진행 day*/
data a; set aa.mgus_outcome_df;
where mm_outcome=1;
format first_c90_date yymmdd8.; 
mm_day = c90_medi_date-mgus_index_date ;
mm_year = mm_day/365.65; 
run;
proc univariate data=a; var mm_day mm_year; run;




/******************************* 산정특례 확인 **************************************/

/***************** MGUS ********************/
/*1-1. V193 한번도 없는 사람*/
proc sql;
create table mgus_v193 as
select jid, count(*) as cnt_v193
from aa.mgus_mm_out
where substr(PRCL_SYM_TP_CD,1,4) = 'V193'
group by jid;
quit;

proc sql;
create table mgus_v_check as
select a.jid, b.cnt_v193 
from (select distinct jid from aa.mgus_mm_out) a 
left join mgus_v193 as b on a.jid=b.jid;
quit;

data mgus_v_check; set mgus_v_check;
if cnt_v193=. then cnt_v193=0; else cnt_v193=cnt_v193; run;
proc freq data=mgus_v_check; table cnt_v193; run;



/******************************* CCI 확인 **************************************/
/* 1. MGUS : aa.MGUS_MM_OUT */
proc sql;
create table mgus_cci as
select jid, 
	/*MI*/
	max(case when (substr(main_sick,1,3) in ('I21','I22') or  substr(main_sick,1,4)='I252') and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as mi_yn1,
	sum(case when (substr(sub_sick,1,3) in ('I21','I22') or  substr(sub_sick,1,4)='I252') and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as mi_yn0, 
	/*CHF*/
	max(case when (substr(main_sick,1,3) in ('I43','I50') or  substr(main_sick,1,4) in ('I099','I110','I130','I132','I255','I420','I425','I426','I427','I428','I429','P290')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as chf_yn1,
    sum(case when (substr(sub_sick,1,3) in ('I43','I50') or  substr(sub_sick,1,4) in ('I099','I110','I130','I132','I255','I420','I425','I426','I427','I428','I429','P290')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as chf_yn0,
    /*PVD*/
    max(case when (substr(main_sick,1,3) in ('I70','I71') or  substr(main_sick,1,4) in ('I731','I738','I739','I771','I790','I792','K551','K558','K559','Z958','Z959')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as pvd_yn1,
    sum(case when (substr(sub_sick,1,3) in ('I70','I71') or  substr(sub_sick,1,4) in ('I731','I738','I739','I771','I790','I792','K551','K558','K559','Z958','Z959')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as pvd_yn0,
    /*CVD*/
    max(case when (substr(main_sick,1,3) in ('G45','G46','I60','I61','I62','I63','I64','I65','I66','I67','I68','I69') or  substr(main_sick,1,4) ='H340') and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as cvd_yn1,
    sum(case when (substr(sub_sick,1,3) in ('G45','G46','I60','I61','I62','I63','I64','I65','I66','I67','I68','I69') or  substr(sub_sick,1,4) ='H340') and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as cvd_yn0,
    /*Dementia*/
    max(case when (substr(main_sick,1,3) in ('F00','F01','F02','F03','G30') or  substr(main_sick,1,4) in ('F051','G311')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as dem_yn1,
    sum(case when (substr(sub_sick,1,3) in ('F00','F01','F02','F03','G30') or  substr(sub_sick,1,4) in ('F051','G311')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as dem_yn0,
    /*CPD*/
	max(case when (substr(main_sick,1,3) in ('J40','J41','J42','J43','J44','J45','J46','J47','J60','J61','J62','J63','J64','J65','J66','J67') or  substr(main_sick,1,4) in ('I278','I279','J684','J701','J703')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as cpd_yn1,
    sum(case when (substr(sub_sick,1,3) in ('J40','J41','J42','J43','J44','J45','J46','J47','J60','J61','J62','J63','J64','J65','J66','J67') or  substr(sub_sick,1,4) in ('I278','I279','J684','J701','J703')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as cpd_yn0,
    /*Rheumatic disease*/
    max(case when (substr(main_sick,1,3) in ('M05','M06','M32','M33','M34') or  substr(main_sick,1,4) in ('M315','M351','M353','M360')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as rhe_yn1,
    sum(case when (substr(sub_sick,1,3) in ('M05','M06','M32','M33','M34') or  substr(sub_sick,1,4) in ('M315','M351','M353','M360')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as rhe_yn0,
    /*PUD*/
    max(case when (substr(main_sick,1,3) in ('K25','K26','K27','K28')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as pud_yn1,
    sum(case when (substr(sub_sick,1,3) in ('K25','K26','K27','K28')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as pud_yn0,
    /*MLD*/
    max(case when (substr(main_sick,1,3) in ('B18','K73','K74') or  substr(main_sick,1,4) in ('K700','K701','K702','K703','K709','K713','K714','K715','K717','K760','K762','K763','K764','K768','K769','Z944')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as mld_yn1,
    sum(case when (substr(sub_sick,1,3) in ('B18','K73','K74') or  substr(sub_sick,1,4) in ('K700','K701','K702','K703','K709','K713','K714','K715','K717','K760','K762','K763','K764','K768','K769','Z944')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as mld_yn0,
    /*DWOC*/
	max(case when (substr(main_sick,1,4) in ('E100','E101','E106','E108','E109','E110','E111','E116','E118','E119','E120','E121','E126','E128','E129','E130','E131','E136','E138','E139','E140','E141','E146','E148','E149')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as dwoc_yn1,
    sum(case when (substr(sub_sick,1,4) in ('E100','E101','E106','E108','E109','E110','E111','E116','E118','E119','E120','E121','E126','E128','E129','E130','E131','E136','E138','E139','E140','E141','E146','E148','E149')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as dwoc_yn0,
    /*DWCC*/
	max(case when (substr(main_sick,1,4) in ('E102','E103','E104','E105','E107','E112','E113','E114','E115','E117','E122','E123','E124','E125','E127','E132','E133','E134','E135','E137','E142','E143','E144','E145','E147')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as dwcc_yn1,
    sum(case when (substr(sub_sick,1,4) in ('E102','E103','E104','E105','E107','E112','E113','E114','E115','E117','E122','E123','E124','E125','E127','E132','E133','E134','E135','E137','E142','E143','E144','E145','E147')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as dwcc_yn0,
    /*Hemiplegia or paraplegia*/
    max(case when (substr(main_sick,1,3) in ('G81','G82') or  substr(main_sick,1,4) in ('G041','G114','G801','G802','G830','G831','G832','G833','G834','G839')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as hp_yn1,
    sum(case when (substr(sub_sick,1,3) in ('G81','G82') or  substr(sub_sick,1,4) in ('G041','G114','G801','G802','G830','G831','G832','G833','G834','G839')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as hp_yn0,
   /*Renal diseases*/
    max(case when (substr(main_sick,1,3) in ('N18','N19') or  substr(main_sick,1,4) in ('I120','I131','N032','N033','N034','N035','N036','N037','N052','N053','N054','N055','N056','N057','N250','Z490','Z491','Z492','Z940','Z992')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as rd_yn1,
    sum(case when (substr(sub_sick,1,3) in ('N18','N19') or  substr(sub_sick,1,4) in ('I120','I131','N032','N033','N034','N035','N036','N037','N052','N053','N054','N055','N056','N057','N250','Z490','Z491','Z492','Z940','Z992')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as rd_yn0,
  /*Any cancer*/
   max(case when (substr(main_sick,1,3) in ('C00','C01','C02','C03','C04','C05','C06','C07','C08','C09',
									'C10','C11','C12','C13','C14','C15','C16','C17','C18','C19',
									'C20','C21','C22','C23','C24','C25','C26','C30','C31','C32',
									'C33','C34','C37','C38','C39','C40','C41','C43','C44','C45',
									'C46','C47','C48','C49','C50','C51','C52','C53','C54','C55',
									'C56','C57','C58','C60','C61','C62','C63','C64','C65','C66',
									'C67','C68','C69','C70','C71','C72','C73','C74','C75','C76',
									'C81','C82','C83','C84','C85',
									'C88','C91','C92','C93','C94','C95','C96','C97','C77','C78','C79','C80')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as cancer_yn1,
    sum(case when (substr(sub_sick,1,3) in ('C00','C01','C02','C03','C04','C05','C06','C07','C08','C09',
									'C10','C11','C12','C13','C14','C15','C16','C17','C18','C19',
									'C20','C21','C22','C23','C24','C25','C26','C30','C31','C32',
									'C33','C34','C37','C38','C39','C40','C41','C43','C44','C45',
									'C46','C47','C48','C49','C50','C51','C52','C53','C54','C55',
									'C56','C57','C58','C60','C61','C62','C63','C64','C65','C66',
									'C67','C68','C69','C70','C71','C72','C73','C74','C75','C76',
									'C81','C82','C83','C84','C85',
									'C88','C91','C92','C93','C94','C95','C96','C97','C77','C78','C79','C80')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as cancer_yn0,
    /*Moderate and severe liver disease*/
    max(case when (substr(main_sick,1,4) in ('I850','I859','I864','I982','K704','K711','K721','K729','K765','K766','K767')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as sld_yn1,
    sum(case when (substr(sub_sick,1,4) in ('I850','I859','I864','I982','K704','K711','K721','K729','K765','K766','K767')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as sld_yn0,
   /*Metastatic solid tumor*/
    max(case when (substr(main_sick,1,3) in ('C77','C78','C79','C80')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as mst_yn1,
    sum(case when (substr(sub_sick,1,3) in ('C77','C78','C79','C80')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as mst_yn0,
   /*AIDS/HIV*/
    max(case when (substr(main_sick,1,3) in ('B20','B21','B22','B24')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as aids_yn1,
    sum(case when (substr(sub_sick,1,3) in ('B20','B21','B22','B24')) and dig_date >= intnx('year',mgus_index_date,-1,'s') then 1 else 0 end) as aids_yn0

 from aa.mgus_mm_out
 group by jid;
 quit;


data aa.mgus_cci_yes; set mgus_cci;
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

proc sql; 
create table aa.mgus_outcci_df as
select *
from aa.mgus_outcome_df as a
left join aa.mgus_cci_yes as b on a.jid=b.jid; 
quit; 

/*진단 시 나이*/
data aa.mgus_outcci_df2; set aa.mgus_outcci_df;
index_age=pat_age + (year(mgus_index_date) - year(dig_date));
run; /*n=7,779*/


/* 19 미만 제외 -> 19 이상 포함 */
data aa.mgus_outcci_df3; set aa.mgus_outcci_df2;
where index_age >=19;
run; *n=5,864;


/* exclude under 19 - version2*/
proc sql;
create table under19 as
select jid, 
		max(case when index_age < 19 then 1 else 0 end) as under19
from aa.mgus_outcci_df2
group by jid
order by jid, index_age;
quit;

proc sort data=under19 nodupkey out=under19_id; by jid; run;
proc freq data=under19_id; table under19; run;

data under19_ex; set under19_id;
where under19=0;
keep jid;
run;


proc sql;
create table mgus_outcci_df3 as
select *
from under19_ex as a
left join aa.mgus_outcci_df2 as b on a.jid=b.jid;
quit; *5,864 ;






/*사망 : DGRSLT_TP_CD = 4 aa.mgus_outcci_death*/
proc sort data=aa.mgus_mm_out; by jid descending dig_end_date; run;

proc sql;
create table mgus_death as
select jid, DGRSLT_TP_CD, dig_end_date, mgus_index_date
from aa.mgus_mm_out
where DGRSLT_TP_CD = '4' and dig_end_date > mgus_index_date
group by jid;
quit;

proc sort data=mgus_death; by jid descending dig_end_date; run;
proc sort data=mgus_death nodupkey out=mgus_death_once(rename=(DGRSLT_TP_CD=death_yn dig_end_date = death_date)); by jid; run;

proc sql;
create table aa.mgus_outcci_death as
select *
from aa.mgus_outcci_df3 as a
left join mgus_death_once as b on a.jid=b.jid;
quit; 
data aa.mgus_outcci_death; set aa.mgus_outcci_death;
if death_yn=4 then death_yn=1 ; else death_yn=0; run; *n=5,864;



/* eliminate death in 6mths from mgus diagnosed date */
proc sql;
create table death_6months as
select jid, death_yn, death_date, mgus_index_date,
	max(case when death_yn = '1' and (death_date - mgus_index_date) <= 180 then 1 else 0 end) as death_6months
from aa.mgus_outcci_death
group by jid
order by jid, death_date;
quit; *n=5,864;

proc sort data=death_6months nodupkey out=death_6months_id; by jid; run;
proc freq data=death_6months_id; table death_6months; run; *n=191;


data death_6months_ex; set death_6months_id;
where death_6months = 0;
keep jid;
run; 

proc sql;
create table aa.mgus_outcci_death2 as
select *
from death_6months_ex as a
left join aa.mgus_outcci_death as b on a.jid=b.jid;
quit; *n=5,673;




/*index year 확인*/
data a; set aa.mgus_outcci_death2;
mgus_index_year = year(mgus_index_date); run;
proc freq data=a; table mgus_index_year*mm_outcome; run;



/*mm 진행 day*/
data a; set aa.mgus_outcci_death2;
where mm_outcome=1;
mm_day = c90_medi_date-mgus_index_date ;
mm_year = mm_day/365.65; 
run;
proc univariate data=a; var mm_day mm_year; run;

data a; set aa.mgus_outcci_death2;
where total_mm_outcome=1;
mm_day = first_c90_date-mgus_index_date ;
mm_year = mm_day/365.65; 
run;
proc univariate data=a; var mm_day mm_year; run;


/*death*/
data aa.mgus_surv; set aa.mgus_outcci_death2;
if death_date=. then death_day =mdy(11,30,2022) - mgus_index_date ;
else if death_yn=1 then death_day= death_date - mgus_index_date;
death_year=death_day/365.65;
run;

proc univariate data=b; var death_year; run;




/******************************* 약물 count **************************************/

/*1. mgus */
proc sql;
create table mgus_drug_check as
select jid, first_c90_date, drug_date,
	max(case when div_cd ='189901ATB' and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as mel_yn, 
	max(case when div_cd in ('463301BIJ', '463302BIJ', '463303BIJ') and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as bor_yn, 
	max(case when div_cd in ('485701ACH', '485702ACH') and mm_outcome=1  and drug_date >= first_c90_date then 1 else 0 end) as thali_yn, 
	max(case when div_cd in ( '588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB', 
				   '588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
				   '588207ACH', '588207ATB') and mm_outcome=1  and drug_date >= first_c90_date then 1 else 0 end) as lenal_yn,
	max(case when div_cd in('48110ATB', '500200ATB', '420732BIJ', '228301ATB', '228303ATB','228305ATB', '228306ALQ', '228307ALQ') and mm_outcome=1  and drug_date >= first_c90_date then 1 else 0 end) as alend_yn,
	max(case when div_cd in ('502201ATB', '502202ATB', '502203ATB', '502204ATB', '111501ATB', '111502ATB', '111503ATB', '111504ATB') and mm_outcome=1  and drug_date >= first_c90_date then 1 else 0 end) as atrova_yn,
	max(case when div_cd in ('527301ATB', '527302ATB') and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as dapag_yn,
	max(case when div_cd in ('628201ATB', '628202ATB') and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as empag_yn,
	max(case when div_cd ='674301ATB' and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as ertug_yn,
	max(case when div_cd in ('480304ATB', '480330BIJ') and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as iband_yn,
	max(case when div_cd in ('191501ATB', '191502ATB', '191502ATR', '191503ATB', '191503ATR', '191504ATB', '191504ATR', '191505ATB', '191505ATR') and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as met_yn,
	max(case when div_cd in ('470901ATB', '470902ATB', '470903ATB') and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as pitava_yn,
	max(case when div_cd in ('216601ATB', '216602ATB', '216603ATB', '216604ATB', '519300ACH') and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as prava_yn,
	max(case when div_cd in ('442302ATB', '442302ATE', '511200ATB', '518400ATB') and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as risedro_yn,
	max(case when div_cd in ('454001ATB', '454001ATD', '454002ATB', '454002ATD', '454003ATB', '454003ATD', '454005ATB') and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as rosuva_yn,
	max(case when div_cd in ('227801ATB', '227801ATR', '227802ATB', '227806ATB') and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as simva_yn,
	max(case when div_cd ='420732BIJ' and mm_outcome=1 and drug_date >= first_c90_date then 1 else 0 end) as zol_yn

from aa.mgus_mm_out
group by jid;
quit;

proc sort data=mgus_drug_check nodupkey out=mgus_drug_check2 (drop=first_c90_date drug_date); by jid; run;

proc sql;
create table aa.mgus_drug_df as
select *
from aa.mgus_outcci_death2 as a
left join mgus_drug_check2 as b on a.jid=b.jid;
quit;

proc freq data=aa.mgus_drug_df; table mel_yn*mm_outcome bor_yn*mm_outcome thali_yn*mm_outcome lenal_yn*mm_outcome alend_yn*mm_outcome atrova_yn*mm_outcome dapag_yn*mm_outcome empag_yn*mm_outcome; run;
proc freq data=aa.mgus_drug_df; table alend_yn*mm_outcome atrova_yn*mm_outcome dapag_yn*mm_outcome empag_yn*mm_outcome ertug_yn*mm_outcome iband_yn*mm_outcome met_yn*mm_outcome pitava_yn*mm_outcome prava_yn*mm_outcome risedro_yn*mm_outcome rosuva_yn*mm_outcome simva_yn*mm_outcome zol_yn*mm_outcome; run;




/*drug check*/
proc sql;
create table drug_check as
select 
	case when div_cd ='189901ATB' then 1 else 0 end as mel_yn, 
	case when div_cd in ('463301BIJ', '463302BIJ', '463303BIJ')  then 1 else 0 end as bor_yn, 
	case when div_cd in ('485701ACH', '485702ACH')  then 1 else 0 end as thali_yn, 
	case when div_cd in ( '588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB', 
				   '588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
				   '588207ACH', '588207ATB') then 1 else 0 end as lenal_yn,
	case when div_cd in ('667101BIJ','667102BIJ') then 1 else 0 end as dara_yn,
	case when div_cd in ('139001ATB') then 1 else 0 end as cyc_yn,
	case when div_cd in ('149401BIJ','149402BIJ','149404BIJ','149405BIJ','149430BIJ','149432BIJ','149433BIJ','149434BIJ') then 1 else 0 end as dox_yn,
	case when div_cd in ('149403BIJ','149406BIJ','149431BIJ','149435BIJ') then 1 else 0 end as lip_yn,
	case when div_cd in ('248001BIJ','248002BIJ','248003BIJ','248030BIJ','248031BIJ','248032BIJ') then 1 else 0 end as vin_yn,
	case when div_cd in ('647801BIJ','647801BIJ','647802BIJ','647802BIJ') then 1 else 0 end as carf_yn,
	case when div_cd in ('628001ACH','628002ACH','628003ACH','628004ACH') then 1 else 0 end as pomal_yn,
	case when div_cd in ('134501BIJ','134502BIJ','134503BIJ','134530BIJ','134531BIJ','134532BIJ','134533BIJ','134534BIJ') then 1 else 0 end as cis_yn,
	case when div_cd in ('157101BIJ','157102ACH','157103BIJ','157104ACH','157104BIJ','157105BIJ','157106BIJ','157107BIJ',
									'157108BIJ','157130BIJ','157131BIJ','157132BIJ','157133BIJ','157134BIJ','157135BIJ','157136BIJ') then 1 else 0 end as etop_yn

from aa.t530_t200_mmall;
quit;

proc freq data=drug_check; table mel_yn bor_yn thali_yn lenal_yn dara_yn cyc_yn dox_yn lip_yn vin_yn carf_yn pomal_yn cis_yn etop_yn; run;


data a; set aa.t530_2023q4_18;
where div_cd in ('463301BIJ', '463302BIJ', '463303BIJ') ; run;

proc freq data=aa.t530_2023q4_18; table div_ty_cd; run;

proc freq data=aa.mgus_outcci_death2; table total_mm_outcome mm_outcome total_mm_outcome*mm_outcome; run;



