libname aa '/vol/userdata13/sta_room417' ;




/**********************************************/
/**********************************************/
/*** 1. Mgus to MM: mgus part에 cci 붙이기 ***/
/* use work library */
data mgus_surv; set aa.mgus_surv; run;

/* drug_date, div_cd, mm_outcome을 drop */
data mgus_surv; set mgus_surv;
drop div_cd drug_date mm_outcome; run;

/* drop existing cci */
data mgus_surv2; set mgus_surv; 
drop mi_yes chf_yes pvd_yes cvd_yes dem_yes cpd_yes rhe_yes pud_yes mld_yes dwoc_yes dwcc_yes hp_yes rd_yes cancer_yes sld_yes mst_yes aids_yes; run;

/* 약물 db에서 mgus_surv jid에 해당하는 애들을 뽑는다. */
proc sql;
create table mgus_mm as
select jid, drug_date, div_cd, drug_age
from aa.t530_t300_mm_2
where jid in (select distinct jid from mgus_surv2); quit;

/* 1-1. mm 약물 정의 (mm 약물 있으면 mm_yn=1로 없으면 mm_yn=0으로 정의) */
data mgus_mm2; set mgus_mm;
if (div_cd in ('189901ATB', '463301BIJ', '463302BIJ', '463303BIJ', '485701ACH', '485702ACH',
									'588201ACH', '588201ATB', '588202ACH', '588202ATB', '588203ACH', '588203ATB',
									'588204ACH', '588204ATB', '588205ACH', '588205ATB', '588206ACH', '588206ATB',
									'588207ACH', '588207ATB')) then mm_yn=1; run;
/* 1-2. mm_yn=. 인 애들은 mm_yn=0으로 해준다. */
data mgus_mm2; set mgus_mm2;
if mm_yn=. then mm_yn=0; run;

/* 1-3. 첫 mm 복용일을 정의한다. */
proc sql;
create table first_mm as
select jid, min(drug_date) as first_mm_date, min(drug_age) as first_mm_age
from mgus_mm2
where mm_yn=1
group by jid; quit;
data first_mm; set first_mm;
format first_mm_date yymmdd8.; run;

/* 1-4. 약물 고유키 출력 */
proc sort data=mgus_mm2; by jid descending mm_yn; run;
proc sort data=mgus_mm2 nodupkey out=mgus_mm_id; by jid; run;

/* 1-5. 약물 고유키 db에 first_mm_date를 붙인다. */
proc sql;
create table mgus_mm3 as
select a.jid, a.mm_yn, b.first_mm_date, b.first_mm_age
from mgus_mm_id as a left join first_mm as b on a.jid=b.jid; quit;

/* mgus_surv에 약물 db를 붙인다. */
proc sql;
create table mgus_surv_drug as
select a.*, b.mm_yn, b.first_mm_date, b.first_mm_age
from mgus_surv2 as a left join mgus_mm3 as b on a.jid=b.jid; quit;

/* death_day, death_year 정의 */
/* 기존의 death_day, death_year <- 냅둔다. 희주샘께서 데이터 매니징하실때 사용하신 거 같음. so, death_day_MM, death_year_MM으로 새로 정의한다. */
data mgus_surv3; set mgus_surv_drug;
if death_yn=0 then death_day_MM = mdy(11,30,2022) - first_mm_date;
else if death_yn=1 then death_day_MM = death_date - first_mm_date;
death_year_MM = death_day_MM/365.65;
run;


/* t20에서 jid, main_sick, sub_sick, first dig date를 가져온다. */
proc sql;
create table mgus_cci as
select jid, main_sick, sub_sick, recu_fr_dd
from aa.t200_2023q4_18
where jid in (select jid from mgus_surv3); quit;
data mgus_cci; set mgus_cci; 
dig_date = mdy(substr(recu_fr_dd,5,2), substr(recu_fr_dd,7,2), substr(recu_fr_dd,1,4)); format dig_date yymmdd10.; run;

proc sql;
create table mgus_cci2 as
select a.*, b.first_mm_date
from mgus_cci as a left join mgus_surv3 as b on a.jid=b.jid; quit;


proc sql;
create table mgus_cci3 as
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

 from mgus_cci2
 group by jid;
 quit;




data mgus_cci_yes; set mgus_cci3;
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
create table aa.mgus_surv_cci as
select *
from mgus_surv3 as a
left join mgus_cci_yes as b on a.jid=b.jid; 
quit; 

