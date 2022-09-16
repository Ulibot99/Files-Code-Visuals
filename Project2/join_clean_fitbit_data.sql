-- Load Data
USE PortfolioProject;

SELECT *
FROM DailyCalories

SELECT *
FROM DailyIntensities

SELECT *
FROM DailySteps

-- Join the 3 Tables and save results into Temp Table
SELECT
	ds.Id,
	ds.ActivityDay AS date,
	ds.StepTotal,
	di.SedentaryMinutes,
	di.LightlyActiveMinutes,
	di.FairlyActiveMinutes AS ModeratelyActiveMinutes,
	di.VeryActiveMinutes,
	di.SedentaryActiveDistance,
	di.LightActiveDistance,
	di.ModeratelyActiveDistance,
	di.VeryActiveDistance,
	dc.Calories
INTO #DailyUserActivity
FROM DailySteps AS ds
JOIN DailyIntensities AS di
ON ds.Id = di.Id AND ds.ActivityDay = di.ActivityDay
JOIN DailyCalories AS dc
ON di.Id = dc.Id AND di.ActivityDay = dc.ActivityDay

-- Check for Duplicates
SELECT 
	UserID,
	COUNT(UserID)
FROM (SELECT
		CONCAT(CAST(Id AS VARCHAR), ':', CAST(date AS VARCHAR)) AS UserID
	  FROM #DailyUserActivity) ui
GROUP BY 
	UserID
HAVING COUNT(UserID) > 1

-- Count number of Users
SELECT
	COUNT(DISTINCT(Id))
FROM #DailyUserActivity

-- Count number of Users Phone usage
SELECT
	Id,
	COUNT(Id) AS count_id
FROM #DailyUserActivity
GROUP BY 
	Id
ORDER BY 
	2 DESC

-- Check for Minutes input accuracy (In 1 day there are 1,440 minutes)
SELECT *
FROM #DailyUserActivity
WHERE (SedentaryMinutes + LightlyActiveMinutes + ModeratelyActiveMinutes + VeryActiveMinutes) > 24*60

-- Check for Negative time
SELECT *
FROM #DailyUserActivity
WHERE SedentaryMinutes < 0 OR LightlyActiveMinutes < 0 OR ModeratelyActiveMinutes < 0 OR VeryActiveMinutes < 0

--Inspect Calories Burned when 0
SELECT *
FROM #DailyUserActivity
WHERE Calories = 0 

-- Inspect StepTotal when 0
SELECT *
FROM #DailyUserActivity
WHERE StepTotal = 0
ORDER BY Calories ASC

--Remove Data where FitBit tracker did not record calories burned
DELETE
FROM #DailyUserActivity
WHERE Calories = 0 AND StepTotal = 0

-- Create new column Total distance
SELECT
	*,
	SedentaryActiveDistance + LightActiveDistance + ModeratelyActiveDistance + VeryActiveDistance AS TotalActiveDistance
FROM #DailyUserActivity




