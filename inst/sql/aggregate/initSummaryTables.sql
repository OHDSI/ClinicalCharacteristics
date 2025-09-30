/* Initialize Continuous Table*/
DROP TABLE IF EXISTS @continuous_table;
CREATE TABLE @continuous_table (
	target_cohort_id BIGINT NOT NULL,
	ordinal_id INT NOT NULL,
	time_label VARCHAR(50) NOT NULL,
	line_item_label VARCHAR(200) NOT NULL,
	patient_line VARCHAR(50) NOT NULL,
	statistic_type VARCHAR(50) NOT NULL,
	subject_count BIGINT NOT NULL,
	mean FLOAT NOT NULL,
	standard_deviation FLOAT NOT NULL,
	minimum_value FLOAT NOT NULL,
	p10_value FLOAT NOT NULL,
	p25_value FLOAT NOT NULL,
	median_value FLOAT NOT NULL,
	p75_value FLOAT NOT NULL,
	p90_value FLOAT NOT NULL,
	maximum_value FLOAT NOT NULL
);

DROP TABLE IF EXISTS @categorical_table_table;
/* Initialize Categorical Table*/
CREATE TABLE @categorical_table (
	target_cohort_id BIGINT NOT NULL,
	ordinal_id INT NOT NULL,
	time_label VARCHAR(50) NOT NULL,
	line_item_label VARCHAR(200) NOT NULL,
	patient_line VARCHAR(50) NOT NULL,
	statistic_type VARCHAR(50) NOT NULL,
	subject_count BIGINT NOT NULL,
	tot_subjects BIGINT NOT NULL,
	pct FLOAT NOT NULL
);
