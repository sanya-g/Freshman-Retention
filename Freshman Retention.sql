USE [WslWarehouse]
GO
/****** Object:  StoredProcedure [dbo].[procLoadFallFreshmanRetention]    Script Date: 4/8/2024 8:43:13 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

ALTER PROCEDURE [dbo].[procLoadFallFreshmanRetention]
AS
BEGIN

/*
------------------------------------------------------------------------------------------------------------------------------
 Source:		DW, Jenzabar, PFAIDS
 Description:	Random Forest Freshman Retention Model 
				This model will automatically flip the "Validation" data flag based on Cohort Year being equal to the Default Year in Jenzabar.
				Builds a dataset to be ran through an R script for a Random Forest algorithm
				This query does everything:
				1) builds the dataset for R
				2) Executes the R script
				3) Ingests the CSV files and PNG images created by the R Script
				4) Ingests the CSV file and updates the FFRetention Table
				5) adds new rows to the archive table
				6) Sets valid and expire dates in the archive table
-----------------------------------------------------------------------------------------------------------------------------
*/

DECLARE @CurrentAcadTerm VARCHAR(6) = (SELECT CONCAT(cur_yr_dflt,cur_trm_dflt) FROM load_REG_CONFIG rc)
DECLARE @CurrentTerm VARCHAR(2) = (SELECT cur_trm_dflt FROM load_REG_CONFIG rc)
DECLARE @CurrentTermKey INT = (SELECT ac.AcademicCalKey FROM dimAcademicCalendar ac WHERE CONCAT(ac.AcadYear,ac.TermCode) = @CurrentAcadTerm)

--Creates pool of FF cohort students.
IF OBJECT_ID('tempdb..#StudentPool') IS NOT NULL DROP TABLE #StudentPool
SELECT
	StudentID = sme.id_num,
	Cohort = RTRIM(sme.cohort_cde),
	CohortAcadTerm = CONCAT(cd.cohort_year,CASE RIGHT(LEFT(sme.cohort_cde,2),1) WHEN 'F' THEN '10' WHEN 'S' THEN '30' WHEN 'M' THEN '40' END),
	AcadTermKey = ac.AcademicCalKey,
	--This will adjust the AcadTermKey to spring during the spring term.  Fall and Spring are the only options
	CurrentTermKey = CASE WHEN @CurrentTerm = '10' THEN ac.AcademicCalKey WHEN @CurrentTerm = '30' THEN ct.AcademicCalKey WHEN @CurrentTerm = '40' THEN ct.AcademicCalKey END
INTO #StudentPool
FROM load_STUDENT_MASTER_EXT sme
LEFT JOIN load_COHORT_DEFINITION cd ON cd.cohort_cde = sme.cohort_cde
LEFT JOIN dimAcademicCalendar ac ON CONCAT(ac.AcadYear, ac.TermCode) = CONCAT(cd.cohort_year,CASE RIGHT(LEFT(sme.cohort_cde,2),1) WHEN 'F' THEN '10' WHEN 'S' THEN '30' WHEN 'M' THEN '40' END)
LEFT JOIN dimAcademicCalendar ct ON ac.AcademicCalKey +1 = ct.AcademicCalKey
WHERE LEFT(sme.cohort_cde,2) = 'FF'
AND cd.cohort_year >= 2015

--Gets meal plans and housing information.  fact_student_term needed because of lost data in STUD_LIFE_CHGS in the 2020/21 Fall term
IF OBJECT_ID('tempdb..#MealsHousing') IS NOT NULL DROP TABLE #MealsHousing
SELECT DISTINCT
	sp.StudentID,
	st.sess_cde,
	HasMealPlan = CASE 
					WHEN st.sess_cde = '2021/10' 
						THEN SUM(CASE WHEN fst.MealPlan > '1'THEN 1 ELSE 0 END)
					ELSE SUM(CASE WHEN slc.meal_plan IS NOT NULL THEN 1 ELSE 0 END)
					END,
	HasRoom = CASE 
					WHEN st.sess_cde = '2021/10'
						THEN SUM(CASE WHEN fst.RoomCode > '1' THEN 1 ELSE 0 END)
					ELSE SUM(CASE WHEN slc.room_cde IS NOT NULL THEN 1 ELSE 0 END)
					END
INTO #MealsHousing
FROM #StudentPool sp
LEFT JOIN load_SESS_TABLE st ON st.yr_cde = LEFT(sp.CohortAcadTerm,4)
								AND st.chg_trm = @CurrentTerm
LEFT JOIN load_YEAR_TERM_TABLE ytt ON CONCAT(ytt.yr_cde, ytt.trm_cde) = CONCAT(LEFT(sp.CohortAcadTerm,4),@CurrentTerm)
LEFT JOIN fact_student_term fst ON fst.StudentID = sp.StudentID
								AND CONCAT(fst.YearCode,fst.TermCode) = CONCAT(LEFT(sp.CohortAcadTerm,4),@CurrentTerm)
						AND DATEADD(dd,-45,ytt.trm_end_dte) BETWEEN fst.dss_start_date AND fst.dss_end_date							
LEFT JOIN load_STUD_LIFE_CHGS slc ON slc.id_num = sp.StudentID
								AND slc.sess_cde = st.sess_cde
GROUP BY sp.StudentID, st.sess_cde
ORDER BY sp.StudentID

IF OBJECT_ID('tempdb..#CurrentCareerGPA') IS NOT NULL DROP TABLE #CurrentCareerGPA
--Two parts, 1st gets last STSD term, 2nd part uses the last STSD Term to get current career GPA and academic probation data
;WITH cteSTSDLast AS (
SELECT
	sp.StudentID,
	LastSTSDTerm = MAX(CONCAT(stsd.yr_cde,stsd.trm_cde))

FROM #StudentPool sp
LEFT JOIN load_STUD_TERM_SUM_DIV stsd ON stsd.id_num = sp.StudentID AND stsd.div_cde = 'UG' AND stsd.transaction_sts IN ('c','h')
WHERE stsd.trm_cde IN ('10','30','40')
AND CONCAT(stsd.yr_cde,stsd.trm_cde) <= @CurrentAcadTerm --(SELECT CONCAT(cur_yr_dflt,cur_trm_dflt) FROM load_REG_CONFIG rc)
GROUP BY sp.StudentID
)

SELECT
	sl.StudentID,
	sl.LastSTSDTerm,
	stsd.career_gpa,
	stsd.academic_probation
INTO #CurrentCareerGPA
FROM cteSTSDLast sl	
LEFT JOIN load_STUD_TERM_SUM_DIV stsd ON stsd.id_num = sl.StudentID AND CONCAT(stsd.yr_cde,stsd.trm_cde) = sl.LastSTSDTerm
ORDER BY sl.LastSTSDTerm

--Looks to see if a student has recieved any military financial aid in their first year
IF OBJECT_ID('tempdb..#MilAid') IS NOT NULL DROP TABLE #MilAid
SELECT
	sp.StudentID,
	RxMilAid = MIN(CASE WHEN th.ID_NUM IS NOT NULL THEN 1 ELSE 0 END)
INTO #MilAid
FROM #StudentPool sp
LEFT JOIN load_TRANS_HIST th ON th.ID_NUM = sp.StudentID
						AND th.CHG_YR_TRAN_HIST = LEFT(sp.CohortAcadTerm,4)
WHERE th.ACCT_CDE = '1    01   00   1000 1030 00'
AND (th.TRANS_DESC like ('%army%') 
	OR th.TRANS_DESC LIKE ('%air%force%') 
	OR th.TRANS_DESC LIKE ('%CH33%') 
	OR th.TRANS_DESC like ('%voc%reh%') 
	OR th.trans_desc LIKE ('%VR&E%')
	)
GROUP BY sp.StudentID

--Gets needed data directly from PowerFAIDS
IF OBJECT_ID('tempdb..#PF') IS NOT NULL DROP TABLE #PF
SELECT DISTINCT
	sp.StudentID,
	say.award_year_token,
	FirstGen = CASE 
					WHEN (MAX(sfs.par_1_highest_grade_level) <> 3 AND MAX(sfs.par_2_highest_grade_level) <> 3) 
						THEN 'Yes'
						ELSE 'No' 
				END,
	PellRecipient = CASE WHEN SUM(CASE WHEN sa.fund_name = 'PELL' THEN 1 ELSE 0 END) >0 THEN 'Yes' ELSE 'No' END,
	PellEligible = CASE WHEN MAX(sfs.fed_pell_grant_eligibility) = 'Y' THEN 'Yes' ELSE 'No' END,
	PrimaryEFC = ISNULL(MAX(sfs.primary_efc),0),
	InstAid = SUM(CASE WHEN f.fund_source = 'I' THEN sa.actual_amt ELSE 0 END),
	PrivAid = SUM(CASE WHEN f.fund_source = 'P' THEN sa.actual_amt ELSE 0 END),
	FedAid = SUM(CASE WHEN f.fund_source = 'F' THEN sa.actual_amt ELSE 0 END),
	FedLoans = SUM(CASE WHEN f.fund_source = 'F' AND f.fund_type = 'L' THEN sa.actual_amt ELSE 0 END),
	NumberLoans = SUM(CASE WHEN f.fund_source = 'F' AND f.fund_type = 'L' THEN 1 ELSE 0 END),
	NumberInstSchol = SUM(CASE WHEN f.fund_source = 'I' THEN 1 ELSE 0 END),
	UnmetNeed = MAX(vfc.unmet_pack_need),
	MilitaryAid = IIF(SUM(CASE WHEN (f.fund_long_name IN ('MILITARY TUITION ASSISTANCE','Military Scholarship','Vocational Rehabilitation') OR ma.StudentID IS NOT NULL) THEN 1 ELSE 0 END) >=1,1,0)

INTO #PF
FROM #StudentPool sp
LEFT JOIN JENZABAR.Pfaids.dbo.student s ON s.alternate_id = sp.StudentID
LEFT JOIN JENZABAR.Pfaids.dbo.stu_award_year say ON say.student_token = s.student_token
												AND say.award_year_token = LEFT(sp.CohortAcadTerm,4)
LEFT JOIN JENZABAR.Pfaids.dbo.say_fm_stu sfs ON sfs.stu_award_year_token = say.stu_award_year_token
LEFT JOIN JENZABAR.Pfaids.dbo.stu_award sa ON sa.stu_Award_year_token = say.stu_award_year_token 
LEFT JOIN JENZABAR.Pfaids.dbo.funds f ON f.fund_token = sa.fund_ay_token
LEFT JOIN JENZABAR.Pfaids.dbo.v_fm_cc vfc ON vfc.stu_award_year_token = say.stu_award_year_token
LEFT JOIN #MilAid ma ON ma.StudentID = sp.StudentID
WHERE sa.status = 'a'
GROUP BY sp.StudentID, say.award_year_token

--Calculates GPA's and hours for each term of attendance
IF OBJECT_ID('tempdb..#GPAs') IS NOT NULL DROP TABLE #GPAs
SELECT
	sp.StudentID,
	sp.CohortAcadTerm,
	AcadTerm = CONCAT(sch.yr_cde, sch.trm_cde),
	MidTermGPA = IIF(SUM(sch.midtrm_qual_pts) = 0,NULL,SUM(sch.midtrm_qual_pts)) / SUM(sch.midtrm_hrs_gpa),
	MTQualPoints = SUM(sch.midtrm_qual_pts),
	MTHrsGPA = SUM(sch.midtrm_hrs_gpa),
	TermGPA = IIF(SUM(sch.qual_pts) = 0,NULL, SUM(sch.qual_pts))/ SUM(sch.hrs_gpa),
	TermQualPoints = SUM(sch.qual_pts),
	TermHoursGPA = SUM(sch.hrs_gpa),
	CreditHours = SUM(sch.hrs_attempted),
	AcadTermKey = ac.AcademicCalKey
INTO #GPAs
FROM #StudentPool sp
LEFT JOIN load_STUDENT_CRS_HIST_V sch ON sch.id_num = sp.StudentID 
									--AND CONCAT(sch.yr_cde,sch.trm_cde) = CONCAT(LEFT(sp.CohortAcadTerm,4),@CurrentTerm)
									AND sch.yr_cde = LEFT(sp.CohortAcadTerm,4)
									AND sch.transaction_sts in ('C','H')
									AND sch.trm_cde in ('10','30')--,'40')
									--AND COALESCE(sch.midtrm_hrs_gpa, sch.hrs_gpa) > 0
LEFT JOIN dimAcademicCalendar ac ON CONCAT(ac.AcadYear, ac.TermCode) = CONCAT(sch.yr_cde, sch.trm_cde)
WHERE sch.yr_cde IS NOT NULL
GROUP BY sp.StudentID, sp.Cohort, sp.CohortAcadTerm, sch.yr_cde, sch.trm_cde, ac.AcademicCalKey

--Identifies the # of majors that a student has had in the past
IF OBJECT_ID('tempdb..#Majors') IS NOT NULL DROP TABLE #Majors
SELECT
	cd.StudentID,
	Majors = COUNT(DISTINCT stsd.major_1)
INTO #Majors
FROM #StudentPool cd
LEFT JOIN load_STUD_TERM_SUM_DIV stsd ON stsd.id_num = cd.StudentID
									AND stsd.yr_cde >= LEFT(cd.CohortAcadTerm,4)
									AND stsd.div_cde = 'UG'
									AND stsd.transaction_sts in ('c','h')
GROUP BY cd.StudentID

--Looks for students that have taken remedial courses in the past
IF OBJECT_ID('tempdb..#Remedial') IS NOT NULL DROP TABLE #Remedial
SELECT DISTINCT sch.id_num
INTO #Remedial
FROM wslwarehouse.dbo.load_STUDENT_CRS_HIST_V sch
WHERE sch.transaction_sts IN ('c','h')
AND sch.trm_cde IN ('10','30','40')
AND (CONCAT(RTRIM(sch.crs_comp1),RTRIM(sch.crs_comp2)) IN ('MAT102','ENG111') OR sch.crs_comp2 < 100)
AND sch.yr_cde >= 2013

--Looks for # of courses, # of dropped courses, and # of withdrawn courses for RF model
IF OBJECT_ID('tempdb..#DWC') IS NOT NULL DROP TABLE #DWC
SELECT DISTINCT
	sp.StudentID,
	sp.Cohort,
	sp.CohortAcadTerm,
	Courses = SUM(CASE WHEN sch.transaction_sts IN ('c','h') THEN 1 ELSE 0 END),
	DroppedCourses = SUM(CASE WHEN sch.drop_dte >= ytt.census_dte THEN 1 ELSE 0 END),
	WithdrawnCourse = SUM(CASE WHEN LEFT(sch.grade_cde,1) = 'W' THEN 1 ELSE 0 END),
	ac.AcademicCalKey
INTO #DWC
FROM #StudentPool sp 
LEFT JOIN dimAcademicCalendar ac ON ac.AcademicCalKey = sp.CurrentTermKey
LEFT JOIN load_STUDENT_CRS_HIST_V sch ON sch.id_num = sp.StudentID
							AND sch.yr_cde = LEFT(sp.CohortAcadTerm,4)
							AND sch.trm_cde = (SELECT cur_trm_dflt FROM load_REG_CONFIG)
LEFT JOIN load_YEAR_TERM_TABLE ytt ON ytt.yr_cde = sch.yr_cde
									AND ytt.trm_cde = sch.trm_cde

GROUP BY sp.StudentID, sp.Cohort, sp.CohortAcadTerm, ac.AcademicCalKey
ORDER BY sp.StudentID


--Checks to see if midterm GPA's have been submitted
IF OBJECT_ID('tempdb..#GPACheck') IS NOT NULL DROP TABLE #GPACheck
SELECT 
	sp.Cohort
	,g.AcadTerm
	,NuminCohort = COUNT(sp.StudentID)
	,NULLGPA = SUM(CASE WHEN g.MidTermGPA IS NULL THEN 1 ELSE 0 END)
	,TestValue = COUNT(sp.StudentID) - SUM(CASE WHEN g.MidTermGPA IS NULL THEN 1 ELSE 0 END)
	,NullFinalGPA = SUM(CASE WHEN g.TermGPA IS NULL THEN 1 ELSE 0 END)
	,FinalTestValue = COUNT(sp.StudentID) - SUM(CASE WHEN g.TermGPA IS NULL THEN 1 ELSE 0 END)
	,FinalPerc = ROUND(CAST(SUM(CASE WHEN g.TermGPA IS NOT NULL THEN 1 ELSE 0 END) AS FLOAT) / COUNT(sp.StudentID), 3,3)
	,MTPerc = ROUND(CAST(SUM(CASE WHEN g.MidTermGPA IS NOT NULL THEN 1 ELSE 0 END) AS FLOAT) / COUNT(sp.StudentID),3,3)

INTO #GPACheck	
FROM #StudentPool sp
LEFT JOIN #GPAs g ON g.StudentID = sp.StudentID
				AND g.AcadTermKey = CASE WHEN @CurrentTerm = '10' THEN sp.AcadTermKey WHEN @CurrentTerm = '30' THEN sp.AcadTermKey + 1 END
				AND RIGHT(g.AcadTerm,2) = @CurrentTerm
LEFT JOIN load_STUD_TERM_SUM_DIV stsd ON stsd.id_num = sp.StudentID
									AND CONCAT(stsd.yr_cde, stsd.trm_cde) = sp.CohortAcadTerm
WHERE sp.CohortAcadTerm = (SELECT CONCAT(cur_yr_dflt,'10') FROM load_REG_CONFIG rc)
AND g.AcadTerm IS NOT null
GROUP BY sp.Cohort, g.AcadTerm


--Checks to see if Career GPA's are valid
IF OBJECT_ID('tempdb..#CareerCheck') IS NOT NULL DROP TABLE #CareerCheck
SELECT
	sp.Cohort
	,sp.CohortAcadTerm
	,Students = COUNT(sp.StudentID)
	,BadCareerGPA = SUM(CASE WHEN NULLIF(stsd.local_gpa,0) IS NULL THEN 1 ELSE 0 END)
	,PercentBad = CAST(SUM(CASE WHEN NULLIF(stsd.local_gpa,0) IS NULL THEN 1 ELSE 0 END) AS FLOAT) / COUNT(sp.studentID)
	,PercentGood = 1 - (CAST(SUM(CASE WHEN NULLIF(stsd.local_gpa,0) IS NULL THEN 1 ELSE 0 END) AS FLOAT) / COUNT(sp.studentID))
INTO #CareerCheck
FROM #StudentPool sp
LEFT JOIN load_STUD_TERM_SUM_DIV stsd ON stsd.id_num = sp.StudentID
									AND CONCAT(stsd.yr_cde, stsd.trm_cde) = CASE @CurrentTerm WHEN '10' THEN sp.CohortAcadTerm WHEN '30' THEN CONCAT(LEFT(sp.CohortAcadTerm,4),'30') END

WHERE sp.CohortAcadTerm = (SELECT CONCAT(cur_yr_dflt,'10') FROM load_REG_CONFIG rc)
GROUP BY sp.Cohort, sp.CohortAcadTerm


--Creates dataset to be used within the R Random Forest Model
IF OBJECT_ID('tempdb..#Temp1') IS NOT NULL DROP TABLE #Temp1
SELECT DISTINCT
	DataType = CASE
				WHEN sp.CohortAcadTerm = (SELECT CONCAT(cur_yr_dflt,'10') FROM load_REG_CONFIG rc)
					THEN 'Validation'
				ELSE 'Training'
				END,
	sp.StudentID,
	sp.Cohort,
	Retained_SecondFall = IIF(sp.StudentID IN (SELECT StudentID FROM factEnrollments WHERE BaseEnrollment = 'Y' AND AcadTerm = CONCAT(LEFT(sp.CohortAcadTerm,4) +1,'10')),'Yes','No'),
	Religion = CASE ISNULL(rel.table_desc,'i')
					WHEN 'Other____________' THEN 'Other'
					WHEN 'i' THEN 'Not Reported'
					ELSE RTRIM(rel.table_desc)
					END,
	HSGPA = aoth.gpa,
	ACTScore = MAX(act.total_compos_score) OVER (PARTITION BY sp.StudentID ORDER BY sp.StudentID),
	FallMidTermGPA = CASE 
						WHEN @CurrentTerm = '10' AND (SELECT NULLIF(MTPerc,0) FROM #GPACheck) > .15
							THEN (SELECT ISNULL(gm.MidTermGPA,0) FROM #GPAs gm WHERE gm.AcadTermKey = sp.AcadTermKey AND gm.StudentID = sp.StudentID)
						WHEN @CurrentTerm = '30'
							THEN (SELECT ISNULL(gm.MidTermGPA,0) FROM #GPAs gm WHERE gm.AcadTermKey = sp.AcadTermKey AND gm.StudentID = sp.StudentID)
						ELSE NULL
						END,
	FallTermGPA =  CASE
						WHEN @CurrentTerm = '10' AND (SELECT NULLIF(MTPerc,0) FROM #GPACheck) > .15
							THEN (SELECT ISNULL(COALESCE(gf.TermGPA,gf.MidTermGPA),0) FROM #GPAs gf WHERE gf.AcadTermKey = sp.AcadTermKey AND gf.StudentID = sp.StudentID)
						WHEN @CurrentTerm = '30'
							THEN (SELECT gf.TermGPA FROM #GPAs gf WHERE gf.AcadTermKey = sp.AcadTermKey AND gf.StudentID = sp.StudentID)
						ELSE NULL
						END,

	SpringMidTermGPA = 
					CASE
						WHEN @CurrentTerm = '10' THEN NULL
						WHEN @CurrentTerm = '30' AND (SELECT MTPerc FROM #GPACheck) < .15
							THEN 1 --Inserts a 1 for all records when no MT GPA's exist
						WHEN @CurrentTerm = '30' AND (SELECT MTPerc FROM #GPACheck) > .15
							THEN (SELECT ISNULL(sm.MidTermGPA,0) FROM #GPAs sm WHERE sm.AcadTermKey = sp.AcadTermKey +1 AND sm.StudentID = sp.StudentID)
						ELSE NULL
					END,
	SpringTermGPA = 
					CASE
						WHEN @CurrentTerm = '10' THEN NULL
						WHEN @CurrentTerm = '30' AND (SELECT FinalPerc FROM #GPACheck) < .15
							THEN 1 --Inserts a 1 for all records when no MT GPA's exist
						WHEN @CurrentTerm = '30' AND (SELECT MTPerc FROM #GPACheck) > .15
							THEN (SELECT ISNULL(COALESCE(st1.TermGPA,st1.MidTermGPA),0) FROM #GPAs st1 WHERE st1.AcadTermKey = sp.AcadTermKey +1 AND st1.StudentID = sp.StudentID)
						ELSE NULL 
					END,

	CurrentCareerGPA = CASE 
						WHEN @CurrentTerm = '10' AND (SELECT PercentGood FROM #CareerCheck) >= .85
							THEN ccg.career_gpa
						WHEN @CurrentTerm = '30' 
							THEN ccg.career_gpa
						ELSE NULL
						END,
	SportsTeam = ISNULL(FIRST_VALUE(st.sports_cde) OVER (PARTITION BY sp.StudentID ORDER BY sp.StudentID ASC, st.sports_cde ASC),'Non Athlete'),
	Major = ISNULL(RTRIM(fst.MajorCode1),'Unkn'),
	Age = DATEDIFF(dd,bm.birth_dte, ytt.trm_begin_dte) / 365.25,
	TotalAid = ISNULL(pf.InstAid,0) + ISNULL(pf.PrivAid,0),
	UnmetNeed = ISNULL(pf.UnmetNeed,0) * -1,
	FirstTermHoursAttempt = ISNULL((SELECT ftha.CreditHours FROM #GPAs ftha WHERE ftha.AcadTermKey = sp.AcadTermKey AND ftha.StudentID = sp.StudentID),0),
	FirstTerm_IND = IIF(sp.StudentID IN (SELECT StudentID FROM factEnrollments WHERE BaseEnrollment = 'Y' AND AcadTerm = sp.CohortAcadTerm),1,0),
	NotCatholic_IND = IIF(rel.table_desc <> 'Catholic',1,0),
	Male_IND = IIF(bm.gender = 'M', 1,0),
	Athlete_IND = IIF(st.id_num IS NOT NULL,1,0),
	Football_IND = IIF(FIRST_VALUE(st.sports_cde) OVER (PARTITION BY sp.StudentID ORDER BY sp.StudentID ASC, st.sports_cde ASC) IN ('MFB','MFBE'),1,0),
	PellRecip_IND = IIF(pf.PellRecipient = 'Yes',1,0),
	PellElig_IND = IIF(pf.PellEligible = 'Yes',1,0),
	FirstGen_IND = IIF(pf.FirstGen = 'Yes',1,0),
	Engineering_IND = IIF(mmd.cip_cde_ext = 'SN',1,0),
	Education_IND = IIF(mmd.cip_cde_ext = 'SE',1,0),
	Business_IND = IIF(mmd.cip_cde_ext = 'SB',1,0),
	Health_IND = IIF(mmd.cip_cde_ext = 'SH',1,0),
	Arts_IND = IIF(mmd.cip_cde_ext = 'SA',1,0),
	NursingMajor_IND = IIF(fst.MajorCode1 = 'NUR',1,0),
	BusinessMajor_IND = IIF(fst.MajorCode1 = 'BUS',1,0),
	PsychMajor_IND = IIF(fst.MajorCode1 = 'PSY',1,0),
	InstAidBelow5k_IND = IIF(pf.InstAid <= 5000,1,0),
	InstAidBelow10k_IND = IIF(pf.InstAid <= 10000,1,0),
	UndecidedMajor_IND = IIF(fst.MajorCode1 IN ('UND','UST','CST','EXP'),1,0),
	ExperimentalLearning_IND = IIF(fst.MajorCode1 = 'EXP',1,0),
	BelowMedianHSGPA_IND = IIF(aoth.gpa < AVG(aoth.gpa) OVER (PARTITION BY sp.Cohort ORDER BY sp.Cohort),1,0),
	FirstTermBelow17_IND = IIF(g.CreditHours < 17,1,0),
	FirstTermBelow12_IND = IIF(g.CreditHours <= 12,1,0),
	TookRemedial_IND = IIF(r.id_num IS NOT NULL,1,0),
	HasUnmetNeed_IND = IIF(ISNULL(pf.UnmetNeed,0) >0,1,0),	
	DroppedAll_IND = IIF((d.Courses - d.WithdrawnCourse) <= 0,1,0),--IIF(D.Courses = D.DroppedCourses,1,0),
	Withdrew_IND = IIF(D.WithdrawnCourse >0,1,0),
	AcademicWarning_IND = IIF(COALESCE(ccg.academic_probation, stsd.academic_probation) IN ('AW') AND @CurrentTerm = '30',1,0),
	SubmittedACT_IND = IIF(act.total_compos_score IS NOT NULL,1,0)
	,International_IND = IIF(bm.visa_type IS NOT NULL,1,0)
	,MealPlan_IND = IIF(mh.HasMealPlan > 0,1,0)
	,Commuter_IND = IIF(mh.HasRoom > 0,0,1)
	,NDResident_IND = IIF(am.state = 'ND',1,0)
	,RXMilAid_IND = IIF(pf.MilitaryAid = 1,1,0)

INTO #Temp1
FROM #StudentPool sp	
LEFT JOIN dimAcademicCalendar ac ON ac.AcademicCalKey = sp.CurrentTermKey
LEFT JOIN #PF pf ON pf.StudentID = sp.StudentID
LEFT JOIN load_BIOGRAPH_MASTER bm ON bm.id_num = sp.StudentID
LEFT JOIN load_TABLE_DETAIL rel ON rel.table_value = bm.religion AND rel.column_name = 'religion'
LEFT JOIN load_ETHNIC_RACE_V erv ON erv.id_num = sp.StudentID
LEFT JOIN load_AD_ORG_TRACKING aoth ON aoth.id_num = sp.StudentID AND aoth.last_high_school = 'Y'
LEFT JOIN load_TEST_SCORES act ON act.id_num = sp.StudentID AND act.tst_cde like ('ACT%')
LEFT JOIN load_SPORTS_TRACKING st ON st.id_num = sp.StudentID 
								AND CONCAT(st.yr_cde, st.trm_cde) = @CurrentAcadTerm--CONCAT(ac.AcadYear, ac.TermCode)
								AND st.sports_cde <> 'CAPEL'
INNER JOIN factEnrollments e ON e.StudentID = sp.StudentID
							AND e.AcadTerm = sp.CohortAcadTerm
							AND e.BaseEnrollment = 'Y'
LEFT JOIN load_DEGREE_HISTORY dh ON dh.id_num = sp.StudentID
								AND dh.seq_num_2 = e.DegSeqNum
LEFT JOIN #Majors m ON m.StudentID = sp.StudentID
LEFT JOIN #GPAs g ON g.StudentID = sp.StudentID AND RIGHT(g.AcadTerm,2) = @CurrentTerm
LEFT JOIN #Remedial r ON r.id_num = sp.StudentID
LEFT JOIN #DWC d ON D.StudentID = sp.StudentID --AND d.AcademicCalKey = @CurrentTermKey
LEFT JOIN load_STUD_TERM_SUM_DIV stsd ON stsd.id_num = sp.StudentID
									AND CONCAT(stsd.yr_cde,stsd.trm_cde) = @CurrentAcadTerm
									AND stsd.div_cde = 'UG'

LEFT JOIN load_YEAR_TERM_TABLE ytt ON CONCAT(ytt.yr_cde,ytt.trm_cde) = CONCAT(ac.AcadYear, ac.TermCode)
LEFT JOIN factEGO e1 ON e1.StudentID = sp.StudentID AND e1.AcadTerm = sp.CohortAcadTerm AND e1.BaseEGO = 'Enrolled'
LEFT JOIN fact_student_term fst ON fst.fact_student_term_key = e1.PointInTimeFactStudentTermKey
LEFT JOIN load_MAJOR_MINOR_DEF mmd ON mmd.major_cde = fst.MajorCode1
LEFT JOIN #CurrentCareerGPA ccg ON ccg.StudentID = sp.StudentID
LEFT JOIN #MealsHousing mh ON mh.StudentID = sp.StudentID
LEFT JOIN load_ADDRESS_MASTER am ON am.id_num = sp.StudentID
									AND am.addr_cde = '*LHP'

ORDER BY sp.Cohort


IF OBJECT_ID('tempdb..#Grades') IS NOT NULL DROP TABLE #Grades
SELECT
	sch.id_num
	,sme.cohort_cde
	,TotalCourses = COUNT(sch.crs_cde)
	,NumOf_A = SUM(CASE WHEN LEFT(ISNULL(sch.grade_cde,sch.midterm_gra_cde),1) IN ('P','A') THEN 1 ELSE 0 END)
	,NumOf_B = SUM(CASE WHEN LEFT(ISNULL(sch.grade_cde,sch.midterm_gra_cde),1) = 'B' THEN 1 ELSE 0 END)
	,NumOf_C = SUM(CASE WHEN LEFT(ISNULL(sch.grade_cde,sch.midterm_gra_cde),1) = 'C' THEN 1 else 0 END)
	,NumOf_D = SUM(CASE WHEN LEFT(ISNULL(sch.grade_cde,sch.midterm_gra_cde),1) = 'D' THEN 1 else 0 END)
	,NumOf_F = SUM(CASE WHEN LEFT(ISNULL(sch.grade_cde,sch.midterm_gra_cde),1) = 'F' THEN 1 else 0 END)
	,NumOf_W = SUM(CASE WHEN LEFT(ISNULL(sch.grade_cde,sch.midterm_gra_cde),1) = 'W' THEN 1 else 0 END)
	,GradeTest = IIF(ISNULL(sch.grade_cde, sch.midterm_gra_cde) = NULL,1,0)
INTO #Grades
FROM load_STUDENT_CRS_HIST_V sch
INNER JOIN load_STUDENT_MASTER_EXT sme ON sme.id_num = sch.id_num
									AND LEFT(sme.cohort_cde,2) = 'FF'
INNER JOIN load_COHORT_DEFINITION cd ON cd.cohort_cde = sme.cohort_cde
									AND cd.cohort_year = sch.yr_cde
									AND sch.trm_cde = (SELECT cur_trm_dflt FROM load_REG_CONFIG rc)
WHERE sch.transaction_sts IN ('c','h')
AND sch.credit_type_cde = 'LT'
AND cd.cohort_year >= 2015

GROUP BY sch.id_num, sme.cohort_cde

IF OBJECT_ID('tempdb..#ABCDGrades') IS NOT NULL DROP TABLE #ABCDGrades
SELECT 
	Students = COUNT(g.id_num)
	,GradeTest = SUM(g.GradeTest)
	,GradesIn = NULLIF(sum(g.GradeTest),0) / count(g.id_num)
INTO #ABCDGrades
FROM #Grades g
WHERE g.cohort_cde = CONCAT('FF',(SELECT RIGHT(cur_yr_dflt,2) FROM load_REG_CONFIG))


IF OBJECT_ID('tempdb..#Holds') IS NOT NULL DROP TABLE #Holds
SELECT 
	StudentID = sme.id_num
	,Cohort = RTRIM(sme.cohort_cde)
	,HasRegHold_IND = IIF( SUM(CASE WHEN hd.HOLD_REG = 'H' THEN 1 ELSE 0 END) >1,1,0)
INTO #Holds
FROM load_STUDENT_MASTER_EXT sme
LEFT JOIN load_COHORT_DEFINITION cd ON cd.cohort_cde = sme.cohort_cde
LEFT JOIN load_HOLD_TRAN ht ON ht.id_num = sme.id_num
						AND CONVERT(DATE,CONCAT(MONTH(GETDATE()),'/',DAY(GETDATE()),'/',YEAR(GETDATE()) - (YEAR(GETDATE()) - cd.cohort_year))) BETWEEN CONVERT(DATE,ht.start_dte) AND CONVERT(DATE,ISNULL(ht.end_dte,'12/31/2099'))
						AND ht.hold_cde <> 'AT'
LEFT JOIN JENZABAR.tmseprd.dbo.HOLDS_DEF hd ON hd.HOLD_CDE = ht.hold_cde
WHERE LEFT(cd.cohort_cde,2) = 'FF'
AND cd.cohort_year >= 2015

GROUP BY sme.id_num, sme.cohort_cde

ORDER BY sme.cohort_cde

IF OBJECT_ID('tempdb..#Temp') IS NOT NULL DROP TABLE #Temp
SELECT 
	t.DataType
	,t.StudentID
	,t.Cohort
	,t.Retained_SecondFall
	,t.Religion
	,HSGPA --= IIF(t.HSGPA is NULL, AVG(t.HSGPA) OVER (PARTITION BY T.cohort ORDER BY T.cohort),T.HSGPA)
	,ACTScore -- = IIF(t.ACTScore IS NULL,AVG(actscore) OVER (PARTITION BY T.Cohort ORDER BY T.Cohort),t.ACTScore)
	,FallMidTermGPA = ISNULL(FallMidTermGPA,0)
	,FallTermGPA = ISNULL(FallTermGPA,0)
	,SpringMidTermGPA = ISNULL(SpringMidTermGPA,0)
	,SpringTermGPA = ISNULL(SpringTermGPA,0)
	,CurrentCareerGPA = ISNULL(t.CurrentCareerGPA,0)
	,t.SportsTeam
	,t.Major
	--,t.Age
	,t.TotalAid
	,t.UnmetNeed
	,t.FirstTermHoursAttempt
	--,t.FirstTerm_IND
	,t.NotCatholic_IND
	,t.Male_IND
	,t.Athlete_IND
	,t.Football_IND
	,t.PellRecip_IND
	,t.PellElig_IND
	,t.FirstGen_IND
	,t.Engineering_IND
	,t.Education_IND
	,t.Business_IND
	,t.Health_IND
	,t.Arts_IND
	,t.NursingMajor_IND
	,t.BusinessMajor_IND
	,t.PsychMajor_IND
	,t.InstAidBelow5k_IND
	,t.InstAidBelow10k_IND
	,t.UndecidedMajor_IND
	--,t.ExperimentalLearning_IND
	,t.BelowMedianHSGPA_IND
	,t.FirstTermBelow17_IND
	,t.FirstTermBelow12_IND
	,t.TookRemedial_IND
	,t.HasUnmetNeed_IND
	,t.DroppedAll_IND
	--,t.Withdrew_IND
	,t.AcademicWarning_IND
	,t.SubmittedACT_IND
	,t.International_IND
	,t.MealPlan_IND
	,t.Commuter_IND
	,t.NDResident_IND
	,t.RXMilAid_IND 
	,LowGPA_IND = CASE (SELECT cur_trm_dflt FROM load_reg_config)
					WHEN '10'
						THEN CASE 
							 WHEN T.FallMidTermGPA IS NULL AND (SELECT NULLIF(MTPerc,0) FROM #GPACheck) < .15
								THEN 1
							 WHEN COALESCE(t.FallTermGPA,t.FallMidTermGPA) <= 2.0
								THEN 1
							 ELSE 0
							 END
					WHEN '30'
						THEN CASE WHEN T.SpringMidTermGPA IS NULL AND (SELECT NULLIF(MTPerc,0) FROM #GPACheck g) < .15
								THEN 1
							 WHEN COALESCE(t.SpringTermGPA,t.SpringMidTermGPA) <= 2.5
							 	THEN 1
							ELSE 0
							END
					ELSE 0
					END
	,NumOf_A = IIF((SELECT GradesIn FROM #ABCDGrades) >0.85, g.NumOf_A, 0)
	,NumOf_B = IIF((SELECT GradesIn FROM #ABCDGrades) >0.85, g.NumOf_B, 0)
	,NumOf_C = IIF((SELECT GradesIn FROM #ABCDGrades) >0.85, g.NumOf_C, 0)
	,NumOf_D = IIF((SELECT GradesIn FROM #ABCDGrades) >0.85, g.NumOf_D, 0)
	,NumOf_F = IIF((SELECT GradesIn FROM #ABCDGrades) >0.85, g.NumOf_F, 0)
	,NumOf_W = IIF((SELECT GradesIn FROM #ABCDGrades) >0.85, g.NumOf_W, 0)
	,h.HasRegHold_IND
INTO #Temp
FROM #Temp1 t	
LEFT JOIN #Grades g ON g.id_num = T.StudentID
					AND g.cohort_cde = t.Cohort
LEFT JOIN #Holds h ON h.StudentID = t.StudentID
--ORDER BY T.Cohort


----creates temp object of the validation data that was modeled for insert into the database
IF OBJECT_ID('tempdb..#retCohort') IS NOT NULL DROP TABLE #retCohort
SELECT * 
INTO #retCohort 
FROM #Temp t
WHERE t.DataType = 'Validation'




--Executes the R Script created for the Random Forest ML model using SELECT * FROM #Temp
EXEC sp_execute_external_script
    @language =N'myR',
    @script=N'
	
library(randomForest)
library(ROCR)
library(caret)
library(varImp)
library(caTools)
library(randomForestExplainer)
library(ggplot2)
library(RCurl)
library(corrplot)
#Imports data from CSV

data.df[data.df=="NULL"] <- NA
data.df <- transform(
  data.df,
  Retained_SecondFall=as.factor(Retained_SecondFall),
  #Religion=as.factor(Religion),
  HSGPA=as.double(HSGPA),
  ACTScore=as.double(ACTScore),
  FallMidTermGPA=as.double(FallMidTermGPA),
  FallTermGPA=as.double(FallTermGPA),
  SpringMidTermGPA=as.double(SpringMidTermGPA),
  SpringTermGPA=as.double(SpringTermGPA),
  CurrentCareerGPA=as.double(CurrentCareerGPA),
  #SportsTeam=as.factor(SportsTeam),
  #Major=as.factor(Major),
  #FirstTerm_IND=as.factor(FirstTerm_IND),
  NotCatholic_IND=as.factor(NotCatholic_IND),
  Male_IND = as.factor(Male_IND),
  Athlete_IND = as.factor(Athlete_IND),
  Football_IND=as.factor(Football_IND),
  PellRecip_IND=as.factor(PellRecip_IND),
  PellElig_IND=as.factor(PellElig_IND),
  FirstGen_IND=as.factor(FirstGen_IND),
  Engineering_IND=as.factor(Engineering_IND),
  Education_IND=as.factor(Education_IND),
  Business_IND=as.factor(Business_IND),
  Health_IND=as.factor(Health_IND),
  Arts_IND=as.factor(Arts_IND),
  NursingMajor_IND=as.factor(NursingMajor_IND),
  BusinessMajor_IND=as.factor(BusinessMajor_IND),
  PsychMajor_IND=as.factor(PsychMajor_IND),
  InstAidBelow5k_IND=as.factor(InstAidBelow5k_IND),
  InstAidBelow10k_IND=as.factor(InstAidBelow10k_IND),
  UndecidedMajor_IND=as.factor(UndecidedMajor_IND),
  #ExperimentalLearning_IND=as.factor(ExperimentalLearning_IND),
  BelowMedianHSGPA_IND=as.factor(BelowMedianHSGPA_IND),
  FirstTermBelow17_IND=as.factor(FirstTermBelow17_IND),
  FirstTermBelow12_IND=as.factor(FirstTermBelow12_IND),
  TookRemedial_IND=as.factor(TookRemedial_IND),
  HasUnmetNeed_IND=as.factor(HasUnmetNeed_IND),
  DroppedAll_IND=as.factor(DroppedAll_IND),
  #Withdrew_IND=as.factor(Withdrew_IND),
  AcademicWarning_IND=as.factor(AcademicWarning_IND),
  #SubmittedACT_IND=as.factor(SubmittedACT_IND),
  International_IND=as.factor(International_IND),
  MealPlan_IND=as.factor(MealPlan_IND),
  Commuter_IND=as.factor(Commuter_IND),
  NDResident_IND=as.factor(NDResident_IND),
  RXMilAid_IND=as.factor(RXMilAid_IND),
  #LowMTGPA_IND=as.factor(LowMTGPA_IND),
  NumOf_A = as.factor(NumOf_A),
  NumOf_B = as.factor(NumOf_B),
  NumOf_C = as.factor(NumOf_C),
  NumOf_D = as.factor(NumOf_D),
  NumOf_F = as.factor(NumOf_F),
  NumOf_W = as.factor(NumOf_W),
  HasRegHold_IND = as.factor(HasRegHold_IND)
)
ret.temp <- data.df

mean_val <- colMeans(ret.temp[,c(6:12)],na.rm = TRUE)
ret.temp$HSGPA[is.na(ret.temp$HSGPA)] <- mean_val[1]
ret.temp$ACTScore[is.na(ret.temp$ACTScore)] <- mean_val[2]
ret.temp$FallMidTermGPA[is.na(ret.temp$FallMidTermGPA)] <- mean_val[3]
ret.temp$FallTermGPA[is.na(ret.temp$FallTermGPA)] <- mean_val[4]
ret.temp$SpringMidTermGPA[is.na(ret.temp$SpringMidTermGPA)] <- mean_val[5]
ret.temp$SpringTermGPA[is.na(ret.temp$SpringTermGPA)] <- mean_val[6]
ret.temp$CurrentCareerGPA[is.na(ret.temp$CurrentCareerGPA)] <- mean_val[7]

ret.comp <- ret.temp
#Sets Retained_SecondFall as factor in dataset
#ret.comp$Retained_SecondFall <- as.factor(ret.comp$Retained_SecondFall)
#Creates training subset
ret.trng <- subset(ret.comp, DataType =="Training")
#Creates a testing subset
#ret.test <- subset(ret.comp, DataType =="Testing")
#creates subset of Cohort data for prediction
ret.cohort <- subset(ret.comp,DataType =="Validation")
#Sets Retained_SecondFall from testing data to NA for predictions
ret.cohort$Retained_SecondFall <- NA

#builds temp DF of training data to include only columns 3 to 47
ret.trainAll <- ret.trng #[,c(1:15,17:44)]

#Splits ret.trainAll DF at 70%
split <- sample.split(ret.trainAll, SplitRatio = 0.8)
#Creates training dataframe
train.df <- subset(ret.trainAll, split =="TRUE")
#Creates testing dataframe
test.df <- subset(ret.trainAll, split == "FALSE")
#Sets seed (random number)
set.seed(1124112)
#Runs Random Forest prediction on training DF
rf <- randomForest(Retained_SecondFall~., data=train.df[,c(4:57)], ntree=500)
#Begins tuning to find variations of tree splits
mtry <- tuneRF(train.df[,-c(1:4)],train.df$Retained_SecondFall, ntreeTry = 500, stepFactor = 1.5, improve = 0.01, trace=TRUE, plot=TRUE)
#finds the best split pint from mtry
best.m <- mtry[mtry[,2] == min(mtry[,2]),1] 	
#Re-runs Random Forest using the best split
rf <- randomForest(Retained_SecondFall~., data=train.df[,c(4:57)], mtry=best.m, importance=TRUE, ntree=500, proximity=TRUE)
#Gets predictions on test data
pred <- predict(rf,newdata = test.df[,c(4:57)]) 
#Gets probabilities on test data
prob <- predict(rf,newdata = test.df[,c(4:57)],type="prob")
#displays confusion matrix of testing data
print(table(test.df[,4],pred))
#Applies RF model to cohort data to get probability
cohort.prob <- predict(rf,newdata = ret.cohort[,c(4:57)], type ="prob")
#Applies RF Model to cohort data to get Prediction
cohort.pred <- predict(rf,newdata = ret.cohort[,c(4:57)]) 	
#Sets prediction column in ret.cohort dataframe
ret.cohort$Prediction <- cohort.pred 	
#Sets Probability colun in ret.cohort dataframe
ret.cohort$Probablility <- cohort.prob[,c(2)] 
#Sets Risk Categories
ret.cohort$RetentionRisk <- ifelse(ret.cohort$Probablility >= .9,"Very Low",
                                   ifelse(ret.cohort$Probablility >= .7,"Low",
                                          ifelse(ret.cohort$Probablility >= .5,"Moderate",
                                                 ifelse(ret.cohort$Probablility >= .2, "High","Very High"))))

#Prepares data for ROC Curve
pred1 = predict(rf,type="prob")
perf = prediction(pred1[,2],train.df$Retained_SecondFall)
auc = performance(perf, "auc")
pred3 = performance(perf, "tpr","fpr")

#Creates mTRY plot
mtryPlot.df <- as.data.frame(mtry)
mtryPlot <- ggplot(mtryPlot.df, aes(x=mtry, y=OOBError, color = "red")) + geom_line(linewidth = 1.5)
print(mtryPlot + labs(y = "Out Of Box Error", x = "Model Tuning Sequence") + ggtitle("Tuning Results") +theme(legend.position = "none"))

#write.csv(ret.cohort,"D:/ROutputs/FTICRetentionModel/RandomForestCohortPred.csv",row.names=FALSE) 
cohort.export <- ret.cohort[,c(2,58:60)]
write.csv(cohort.export,"D:/ROutputs/FTICRetentionModel/CurrentCohortResults.csv",row.names=FALSE)

#Creates dataframe of importance data
importance.df <- as.data.frame(importance(rf))
importance.df$Factor <- row.names(importance.df) 
importance.df <- importance.df[,c(5,1,2,3,4)]
#writes random forest importance to csv
write.csv(importance.df,"D:/ROutputs/FTICRetentionModel/Importance.csv",row.names=FALSE)

#creates importance chart visualization and saves as PNG
png(file="D:/ROutputs/FTICRetentionModel/ImportancePlot.png",
    width=800, height=800)
varImpPlot(rf, sort=TRUE,n.var=53, main = "Random Forest Model Importance")
dev.off()

png(file="D:/ROutputs/FTICRetentionModel/mtryPlot.png",
    width = 800, height = 800)
print(mtryPlot + labs(y = "Out Of Box Error", x = "Model Tuning") + ggtitle("Tuning Results") +theme(legend.position = "none", text = element_text(size = 20)))
dev.off()

#outputs ROC Curve as PNG
png(file="D:/ROutputs/FTICRetentionModel/ROCCurve.png",
    width=800, height=800)
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2) 
abline(a=0,b=1,lwd=2,lty=2,col="gray") 	#Plots ROC Curve
dev.off()

#Tree Size to PNG
png(file="D:/ROutputs/FTICRetentionModel/treesize.png",
    width=800, height=800)
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
dev.off()

#MDS Plot to PNG
png(file="D:/ROutputs/FTICRetentionModel/MDSPlot.png",
    width=800, height = 800)
MDSplot(rf, train.df$Retained_SecondFall)
dev.off()


#Outputs Confusion Matrix as png
cm <- confusionMatrix(data=test.df[,4] ,reference=pred, positive ="Yes")
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt="n", yaxt="n")
  title("CONFUSION MATRIX", cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col="#3F97D0")
  text(195, 435, "No", cex=1.2)
  rect(250, 430, 340, 370, col="#F7AD50")
  text(295, 435, "Yes", cex=1.2)
  text(125, 370, "Predicted", cex=1.3, srt=90, font=2)
  text(245, 450, "Actual", cex=1.3, font=2)
  rect(150, 305, 240, 365, col="#F7AD50")
  rect(250, 305, 340, 365, col="#3F97D0")
  text(140, 400, "No", cex=1.2, srt=90)
  text(140, 335, "Yes", cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col="white")
  text(195, 335, res[2], cex=1.6, font=2, col="white")
  text(295, 400, res[3], cex=1.6, font=2, col="white")
  text(295, 335, res[4], cex=1.6, font=2, col="white")
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt="n", yaxt="n")
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  
png(file="D:/ROutputs/FTICRetentionModel/ConfusionMatrix.png")
#fourfoldplot(as.table(cf),color=c("red","green"),main = "Confusion Matrix")
draw_confusion_matrix(cm)
dev.off()

#runs explainer for random forest mode and outputs to Destination Folder folder
#explain_forest(rf, path="D:/ROutputs/FTICRetentionModel/FFRetModelExplained",interactions = TRUE, data = ret.trng[,c(4:51)]) 
#summary(rf)



#Importance Plot into Binary for database
library(RCurl)
txt <- base64Encode(readBin("D:/ROutputs/FTICRetentionModel/ImportancePlot.png", "raw", file.info("D:/ROutputs/FTICRetentionModel/ImportancePlot.png")[1, "size"]), "txt")
impPlt <- sprintf("data:image/png;base64,%s",txt)
id <- c(1)
name <- c("Importance Plot")
blob <- impPlt
df <-data.frame(id,name,blob)

#ROC Curve into Binary for Database
plotRC <- base64Encode(readBin("D:/ROutputs/FTICRetentionModel/ROCCurve.png", "raw", file.info("D:/ROutputs/FTICRetentionModel/ROCCurve.png")[1, "size"]), "txt")
rcPlot <- sprintf("data:image/png;base64,%s",plotRC)
id3 <- c(3)
name3 <- c("ROC Curve")
blob3 <- rcPlot
df[nrow(df) +1,] = c(id3,name3,blob3)

#Tree size plot into Binary for Database
plot_tr <- base64Encode(readBin("D:/ROutputs/FTICRetentionModel/treesize.png", "raw", file.info("D:/ROutputs/FTICRetentionModel/treesize.png")[1, "size"]), "txt")
trPlot <- sprintf("data:image/png;base64,%s",plot_tr)
id4 <- c(4)
name4 <- c("Tree Size")
blob4 <- trPlot
df[nrow(df) +1,] = c(id4,name4,blob4)

#Confusion Matrix into Binary for Database
plot_cm <- base64Encode(readBin("D:/ROutputs/FTICRetentionModel/ConfusionMatrix.png", "raw", file.info("D:/ROutputs/FTICRetentionModel/ConfusionMatrix.png")[1, "size"]), "txt")
cmPlot <- sprintf("data:image/png;base64,%s",plot_cm)
id4 <- c(4)
name4 <- c("Confusion Matrix")
blob4 <- cmPlot
df[nrow(df) +1,] = c(id4,name4,blob4)

#mTry Plot to binary for Database
plot_mt <- base64Encode(readBin("D:/ROutputs/FTICRetentionModel/mtryPlot.png", "raw", file.info("D:/ROutputs/FTICRetentionModel/mtryPlot.png")[1, "size"]), "txt")
mtPlot <- sprintf("data:image/png;base64,%s",plot_mt)
id5 <- c(5)
name5 <- c("Model Tuning")
blob5 <- mtPlot
df[nrow(df) +1,] = c(id5,name5,blob5)

write.csv(df,"D:/ROutputs/FTICRetentionModel/images.csv",row.names=FALSE) 


	;',
	@input_data_1 = N'SELECT * FROM #Temp',
	@input_data_1_name = N'data.df'

;
--Brings results from Random Forest model in as a CSV file that was created by the R Script
IF OBJECT_ID('tempdb..#CurrentCohort') IS NOT NULL DROP TABLE #CurrentCohort
CREATE TABLE #CurrentCohort (StudentID INT, Prediction VARCHAR(5), Probability DECIMAL(18,3), RetentionRisk VARCHAR(20))
BULK INSERT #CurrentCohort
FROM 'D:\ROutputs\FTICRetentionModel\CurrentCohortResults.csv'
	WITH
		(
		FIRSTROW = 2,
		FIELDTERMINATOR = ',',
		ROWTERMINATOR = '0x0a'
		)
--Cleans the imported CSV file to get rid of the leading and trailing " (quotes)
IF OBJECT_ID('tempdb..#CleanedImport') IS NOT NULL DROP TABLE #CleanedImport
SELECT DISTINCT
	cc.StudentID
	,Prediction = REPLACE(cc.Prediction,'"','')
	,cc.Probability
	,RetentionRisk = REPLACE(cc.RetentionRisk,'"','')
INTO #CleanedImport
FROM #CurrentCohort cc

;
--Brings image files that were created by the R Script into datawarehouse (DEV dB) for use in PowerBI report
IF OBJECT_ID('tempdb..#RFImages') IS NOT NULL DROP TABLE #RFImages
CREATE TABLE #RFImages (id varchar(100), name varchar(100), image varchar(MAX))
BULK INSERT #RFImages
FROM 'D:\ROutputs\FTICRetentionModel\images.csv'
	WITH
		(
		FIRSTROW = 2,
		FIELDTERMINATOR = ',',
		ROWTERMINATOR = '0x0a'
		)

TRUNCATE TABLE dev.dbo.RFImages
INSERT INTO dev.dbo.RFImages (id, name, image)

SELECT DISTINCT
	id = REPLACE(cc.id,'"','')
	,Name = REPLACE(cc.name,'"','')
	,Image = CAST(REPLACE(cc.image,'"','') as varchar(max))

FROM #RFImages cc
;
--Deletes existing records from FallFreshmanRetetion that have been modeled for the current cohort
DELETE FROM FallFreshmanRetention
FROM #CleanedImport ci
WHERE ci.StudentID = FallFreshmanRetention.StudentID

--ETL's Dataset ran through the R Random Forest model for use in BI Reporting
INSERT INTO FallFreshmanRetention (StudentID, LastName, FirstName, Cohort, Retained_SecondFall, Prediction, Probability, RetentionRisk, Religion, Gender, HSGPA, ACTScore, FallMidTermGPA, FallTermGPA, SpringMidTermGPA, SpringTermGPA, CurrentCareerGPA, Athlete, SportsTeam, School, Major, Age, FirstTermHoursAttempt, OnAcademicWarning, FirstGen, PellEligible, PellRecipient, InstAid, UnmetNeed, MilitaryAid, MilitaryStatus, TookRemedial, DroppedAllCourses, WithdrewFromCourse, Commuter, HasMealPlan, NDResident, Advisor1, Advisor2, Advisor3)

SELECT DISTINCT
	c.StudentID
	,LastName = RTRIM(nm.last_name)
	,FirstName = RTRIM(nm.first_name)
	,t1.Cohort
	,Retained_SecondFall = NULL
	,ci.Prediction
	,ci.Probability
	,ci.RetentionRisk
	,t1.Religion
	,Gender = CASE bm.gender WHEN 'M' THEN 'Male' WHEN 'F' THEN 'Female' ELSE 'Unknown' END
	,t1.HSGPA
	,t1.ACTScore
	,FallMidTermGPA = fg.MidTermGPA
	,FallTermGPA = fg.TermGPA
	,SpringMidTermGPA = sg.MidTermGPA
	,SpringTermGPA = sg.TermGPA
	,t1.CurrentCareerGPA
	,Athlete = IIF(c.Athlete_IND = 1,'Yes','No')
	,c.SportsTeam
	,School = sd.Initials
	,c.Major
	,Age = ROUND(t1.Age,2,1)
	,c.FirstTermHoursAttempt
	,OnAcademicWarning = IIF(c.AcademicWarning_IND = 1,'Yes','No')
	,P.FirstGen
	,P.PellEligible
	,P.PellRecipient
	,P.InstAid
	,P.UnmetNeed
	,MilitaryAid = IIF(P.MilitaryAid = 1,'Yes','No')
	,MilitaryStatus = CASE 
						WHEN c.StudentID in (SELECT id_num from dbo.load_MILITARY_SERVICE_MASTER where vet_act_dep_service_injury = 'Y' and veteran = 'Y') THEN 'Disabled Vet'
						WHEN c.studentid IN (select id_num from dbo.load_MILITARY_SERVICE_MASTER where is_active_duty_armed_svcs = 'Y') THEN 'Active Duty'
						WHEN c.StudentID IN (select id_num from dbo.load_MILITARY_SERVICE_MASTER where veteran = 'Y') THEN 'Veteran'
						WHEN c.StudentID in (SELECT id_num from dbo.load_MILITARY_SERVICE_MASTER where vet_act_dep_service_injury = 'Y' and veteran = 'N') THEN 'Sp/Dep of DisVet'
						WHEN c.studentid in (select id_num from dbo.load_MILITARY_SERVICE_MASTER where spouse_dep_of_deceased_vet = 'Y') THEN 'Sp of Dec Vet'
						WHEN c.StudentID in (Select id_num from dbo.load_MILITARY_SERVICE_MASTER where spouse_dep_of_vet_act = 'Y') THEN 'Sp of AD/Vet'
						WHEN c.StudentID in (select id_num from dbo.load_MILITARY_SERVICE_MASTER where veteran = 'N' and (active_duty_armed_svcs_branch is not null or veteran_of_armed_svcs_branch is not null)) THEN 'Non-Veteran'	
						ELSE 'N/A'
							END
	,TookRemedial = IIF(t1.TookRemedial_IND = 1,'Yes','No')
	,DroppedAllCourses = IIF(t1.DroppedAll_IND = 1,'Yes','No')
	,WithdrewFromCourse = IIF(t1.Withdrew_IND = 1,'Yes','No')
	,Commuter = IIF(c.Commuter_IND = 1,'Yes','No')
	,HasMealPlan = IIF(c.MealPlan_IND = 1,'Yes','No')
	,NDResident = IIF(c.NDResident_IND = 1,'Yes','No')
	,Advisor1 = CONCAT(RTRIM(ad1.first_name),' ',RTRIM(Ad1.last_name))
	,Advisor2 = CONCAT(RTRIM(ad2.first_name),' ',RTRIM(ad2.last_name))
	,Advisor3 = CONCAT(RTRIM(Ad3.first_name),' ',RTRIM(ad3.last_name))

FROM #retCohort c
LEFT JOIN #CleanedImport ci ON ci.StudentID = c.StudentID
LEFT JOIN #Temp t ON t.StudentID = c.StudentID
LEFT JOIN #Temp1 t1 ON t1.StudentID = c.StudentID
LEFT JOIN #PF p ON P.StudentID = c.StudentID
LEFT JOIN load_BIOGRAPH_MASTER bm ON bm.id_num = c.StudentID
LEFT JOIN #GPAs sg ON sg.StudentID = c.StudentID
					AND LEFT(sg.AcadTerm,4) = LEFT(sg.CohortAcadTerm,4)
					AND RIGHT(sg.AcadTerm,2) = '30'
LEFT JOIN load_NAME_MASTER nm ON nm.id_num = c.StudentID
LEFT JOIN load_STUDENT_DIV_MAST sdm ON sdm.id_num = c.StudentID
									AND sdm.div_cde = 'UG'
LEFT JOIN load_NAME_MASTER ad1 ON ad1.id_num = sdm.advisor_id_num
LEFT JOIN load_NAME_MASTER ad2 ON ad2.id_num = sdm.advisor_id_num_2
LEFT JOIN load_NAME_MASTER ad3 ON ad3.id_num = sdm.advisor_id_num_3
LEFT JOIN load_MAJOR_MINOR_DEF mmd ON mmd.major_cde = c.Major
LEFT JOIN viewSchoolDesc sd ON sd.CIP_CDE_EXT = mmd.cip_cde_ext
LEFT JOIN #GPAs fg ON fg.StudentID = c.StudentID
					AND RIGHT(fg.AcadTerm,2) = '10'




--Updated FallFreshmanRetention Retained_SecondFall flag as students register for the fall term (only updates during Summer term)
IF OBJECT_ID('tempdb..#Update') IS NOT NULL DROP TABLE #Update
SELECT 
	ffr.StudentID
	,ffr.Cohort
	,cd.cohort_year
	,Retained_SecondFall = IIF(ffr.StudentID IN (SELECT StudentID FROM factEnrollments WHERE AcadYear = cd.cohort_year +1 AND TermCode = '10' AND Division = 'UG' AND BaseEnrollment = 'Y'),'Yes','No')

INTO #Update
FROM FallFreshmanRetention ffr
LEFT JOIN load_COHORT_DEFINITION cd ON cd.cohort_cde = ffr.Cohort
WHERE cd.cohort_year <= (SELECT cur_yr_dflt FROM load_REG_CONFIG rc)
AND (SELECT cur_trm_dflt FROM load_REG_CONFIG rc) = '40'

--Updates table's Retained_SecondFall flag using the above #Temp table
UPDATE FallFreshmanRetention
SET Retained_SecondFall = u.Retained_SecondFall
FROM #Update u
WHERE U.StudentID = FallFreshmanRetention.StudentID


--Updates CurrentRecord flag in archive table when differences in student's probability exists
UPDATE FallFreshmanRetention_Archive
SET CurrentRecord = 'N'
	,RecordEndDate = CONVERT(SMALLDATETIME,CONCAT(CONVERT(DATE,DATEADD(dd,-1,GETDATE())),' 23:59:00.000'))
FROM FallFreshmanRetention ffr
WHERE ffr.StudentID = FallFreshmanRetention_Archive.StudentID
AND ffr.Probability <> FallFreshmanRetention_Archive.Probability


--Inserts new records with a different probability into archive table
INSERT INTO FallFreshmanRetention_Archive (StudentID, LastName, FirstName, Cohort, Retained_SecondFall, Prediction, Probability, RetentionRisk, Religion, Gender, HSGPA, ACTScore, FallMidTermGPA, FallTermGPA, SpringMidTermGPA, SpringTermGPA, CurrentCareerGPA, Athlete, SportsTeam, School, Major, Age, FirstTermHoursAttempt, OnAcademicWarning, FirstGen, PellEligible, PellRecipient, InstAid, UnmetNeed, MilitaryAid, MilitaryStatus, TookRemedial, DroppedAllCourses, WithdrewFromCourse, Commuter, HasMealPlan, NDResident, Advisor1, Advisor2, Advisor3, ModelRunDate, RecordStartDate, RecordEndDate, CurrentRecord)
	
SELECT 
	ffr.StudentID
	,ffr.LastName
	,ffr.FirstName
	,ffr.Cohort
	,ffr.Retained_SecondFall
	,ffr.Prediction
	,ffr.Probability
	,ffr.RetentionRisk
	,ffr.Religion
	,ffr.Gender
	,ffr.HSGPA
	,ffr.ACTScore
	,ffr.FallMidTermGPA
	,ffr.FallTermGPA
	,ffr.SpringMidTermGPA
	,ffr.SpringTermGPA
	,ffr.CurrentCareerGPA
	,ffr.Athlete
	,ffr.SportsTeam
	,ffr.School
	,ffr.Major
	,ffr.Age
	,ffr.FirstTermHoursAttempt
	,ffr.OnAcademicWarning
	,ffr.FirstGen
	,ffr.PellEligible
	,ffr.PellRecipient
	,ffr.InstAid
	,ffr.UnmetNeed
	,ffr.MilitaryAid
	,ffr.MilitaryStatus
	,ffr.TookRemedial
	,ffr.DroppedAllCourses
	,ffr.WithdrewFromCourse
	,ffr.Commuter
	,ffr.HasMealPlan
	,ffr.NDResident
	,ffr.Advisor1
	,ffr.Advisor2
	,ffr.Advisor3
	,ModelRunDate = DATEADD(dd,-0,GETDATE())
	,RecordStartDate = CONVERT(SMALLDATETIME,CONCAT(CONVERT(DATE,DATEADD(dd,-0,GETDATE())),' 00:00:00.000')) -- DATEADD(dd,-90,GETDATE()) --
	,RecordEndDate = '2999-12-31 00:00:00.000' 
	,CurrentRecord = 'Y'

FROM FallFreshmanRetention ffr
WHERE CONCAT(ffr.StudentID,'-',ffr.Probability) NOT IN (SELECT CONCAT(StudentID,'-',probability) FROM FallFreshmanRetention_Archive ffra)

--Updates RecordEndDate for non-current records to be one minute behind the new record
UPDATE FallFreshmanRetention_Archive
SET RecordEndDate = DATEADD(MINUTE,-1,(SELECT MAX(fa.RecordStartDate) FROM FallFreshmanRetention_Archive fa WHERE fa.StudentID = FallFreshmanRetention_Archive.StudentID AND fa.CurrentRecord = 'Y'))
WHERE CurrentRecord = 'N'

--Updates RecordEndDate to be in the future for those new records
UPDATE FallFreshmanRetention_Archive
SET RecordEndDate = '2999-12-31 00:00:00.000'
WHERE CurrentRecord = 'Y'
AND RecordEndDate IS NULL



END

