-- Database initialization script for Internet Structure Explorer
-- This script creates the necessary tables and indexes for the application

-- Create database if it doesn't exist (handled by POSTGRES_DB env var)
-- The database is created automatically by PostgreSQL with the POSTGRES_DB variable

-- Create schema for internet structure data
CREATE SCHEMA IF NOT EXISTS internet_structure;

-- Set search path to our schema
SET search_path TO internet_structure, public;

-- Table for storing raw IP-ASN mapping data
CREATE TABLE IF NOT EXISTS ip_asn_mappings (
    id SERIAL PRIMARY KEY,
    start_ip INET NOT NULL,
    end_ip INET NOT NULL,
    asn BIGINT NOT NULL CHECK (asn >= 0 AND asn <= 4294967295),
    organization TEXT,
    data_source VARCHAR(50) NOT NULL DEFAULT 'unknown',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(start_ip, end_ip, asn)
);

-- Table for storing geographic data
CREATE TABLE IF NOT EXISTS geographic_data (
    id SERIAL PRIMARY KEY,
    network CIDR NOT NULL,
    country_code VARCHAR(2),
    country_name VARCHAR(100),
    continent_code VARCHAR(2),
    continent_name VARCHAR(50),
    city_name VARCHAR(100),
    latitude DECIMAL(10, 8),
    longitude DECIMAL(11, 8),
    data_source VARCHAR(50) NOT NULL DEFAULT 'unknown',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(network)
);

-- Table for storing AS information
CREATE TABLE IF NOT EXISTS as_information (
    asn BIGINT PRIMARY KEY CHECK (asn >= 0 AND asn <= 4294967295),
    organization TEXT,
    as_description TEXT,
    as_type VARCHAR(20),
    country_code VARCHAR(2),
    country_name VARCHAR(100),
    website TEXT,
    data_source VARCHAR(50) NOT NULL DEFAULT 'unknown',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Table for storing traceroute results
CREATE TABLE IF NOT EXISTS traceroute_results (
    id SERIAL PRIMARY KEY,
    target_ip INET NOT NULL,
    target_hostname TEXT,
    source_ip INET,
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    protocol VARCHAR(10) DEFAULT 'icmp',
    max_hops INTEGER DEFAULT 30,
    total_hops INTEGER,
    status VARCHAR(20) DEFAULT 'completed',
    error_message TEXT
);

-- Table for storing individual traceroute hops
CREATE TABLE IF NOT EXISTS traceroute_hops (
    id SERIAL PRIMARY KEY,
    traceroute_id INTEGER REFERENCES traceroute_results(id) ON DELETE CASCADE,
    hop_number INTEGER NOT NULL,
    ip_address INET,
    hostname TEXT,
    asn BIGINT CHECK (asn >= 0 AND asn <= 4294967295),
    rtt1 DECIMAL(8, 3),
    rtt2 DECIMAL(8, 3),
    rtt3 DECIMAL(8, 3),
    avg_rtt DECIMAL(8, 3),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(traceroute_id, hop_number)
);

-- Table for storing AS relationship data
CREATE TABLE IF NOT EXISTS as_relationships (
    id SERIAL PRIMARY KEY,
    asn1 BIGINT NOT NULL CHECK (asn1 >= 0 AND asn1 <= 4294967295),
    asn2 BIGINT NOT NULL CHECK (asn2 >= 0 AND asn2 <= 4294967295),
    relationship VARCHAR(20) NOT NULL CHECK (relationship IN ('peer', 'provider', 'customer')),
    data_source VARCHAR(50) NOT NULL DEFAULT 'unknown',
    confidence_score DECIMAL(3, 2) DEFAULT 1.0,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(asn1, asn2, data_source)
);

-- Table for storing connectivity metrics
CREATE TABLE IF NOT EXISTS connectivity_metrics (
    id SERIAL PRIMARY KEY,
    metric_name VARCHAR(100) NOT NULL,
    metric_value JSONB,
    calculation_date DATE NOT NULL DEFAULT CURRENT_DATE,
    data_source VARCHAR(50) NOT NULL DEFAULT 'unknown',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(metric_name, calculation_date, data_source)
);

-- Table for storing data collection logs
CREATE TABLE IF NOT EXISTS data_collection_logs (
    id SERIAL PRIMARY KEY,
    operation_type VARCHAR(50) NOT NULL,
    data_source VARCHAR(50) NOT NULL,
    records_processed INTEGER DEFAULT 0,
    records_inserted INTEGER DEFAULT 0,
    records_updated INTEGER DEFAULT 0,
    records_failed INTEGER DEFAULT 0,
    execution_time INTERVAL,
    status VARCHAR(20) DEFAULT 'running' CHECK (status IN ('running', 'completed', 'failed')),
    error_message TEXT,
    started_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    completed_at TIMESTAMP WITH TIME ZONE
);

-- Create indexes for better query performance
CREATE INDEX IF NOT EXISTS idx_ip_asn_mappings_asn ON ip_asn_mappings(asn);
CREATE INDEX IF NOT EXISTS idx_ip_asn_mappings_start_ip ON ip_asn_mappings USING gist(start_ip inet_ops);
CREATE INDEX IF NOT EXISTS idx_ip_asn_mappings_end_ip ON ip_asn_mappings USING gist(end_ip inet_ops);
CREATE INDEX IF NOT EXISTS idx_ip_asn_mappings_data_source ON ip_asn_mappings(data_source);

CREATE INDEX IF NOT EXISTS idx_geographic_data_network ON geographic_data USING gist(network inet_ops);
CREATE INDEX IF NOT EXISTS idx_geographic_data_country ON geographic_data(country_code);
CREATE INDEX IF NOT EXISTS idx_geographic_data_continent ON geographic_data(continent_code);

CREATE INDEX IF NOT EXISTS idx_as_information_country ON as_information(country_code);
CREATE INDEX IF NOT EXISTS idx_as_information_type ON as_information(as_type);

CREATE INDEX IF NOT EXISTS idx_traceroute_results_target ON traceroute_results(target_ip);
CREATE INDEX IF NOT EXISTS idx_traceroute_results_timestamp ON traceroute_results(timestamp);
CREATE INDEX IF NOT EXISTS idx_traceroute_results_status ON traceroute_results(status);

CREATE INDEX IF NOT EXISTS idx_traceroute_hops_traceroute_id ON traceroute_hops(traceroute_id);
CREATE INDEX IF NOT EXISTS idx_traceroute_hops_asn ON traceroute_hops(asn);
CREATE INDEX IF NOT EXISTS idx_traceroute_hops_ip ON traceroute_hops(ip_address);

CREATE INDEX IF NOT EXISTS idx_as_relationships_asn1 ON as_relationships(asn1);
CREATE INDEX IF NOT EXISTS idx_as_relationships_asn2 ON as_relationships(asn2);
CREATE INDEX IF NOT EXISTS idx_as_relationships_relationship ON as_relationships(relationship);

CREATE INDEX IF NOT EXISTS idx_connectivity_metrics_name ON connectivity_metrics(metric_name);
CREATE INDEX IF NOT EXISTS idx_connectivity_metrics_date ON connectivity_metrics(calculation_date);

CREATE INDEX IF NOT EXISTS idx_data_collection_logs_operation ON data_collection_logs(operation_type);
CREATE INDEX IF NOT EXISTS idx_data_collection_logs_status ON data_collection_logs(status);
CREATE INDEX IF NOT EXISTS idx_data_collection_logs_started ON data_collection_logs(started_at);

-- Create a function to update the updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Create triggers to automatically update updated_at columns
CREATE TRIGGER update_ip_asn_mappings_updated_at
    BEFORE UPDATE ON ip_asn_mappings
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_geographic_data_updated_at
    BEFORE UPDATE ON geographic_data
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_as_information_updated_at
    BEFORE UPDATE ON as_information
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_as_relationships_updated_at
    BEFORE UPDATE ON as_relationships
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- Create a view for unified AS data
CREATE OR REPLACE VIEW unified_as_data AS
SELECT
    iam.asn,
    iam.organization,
    iam.start_ip,
    iam.end_ip,
    gd.country_code,
    gd.country_name,
    gd.continent_code,
    gd.continent_name,
    gd.city_name,
    gd.latitude,
    gd.longitude,
    ai.as_description,
    ai.as_type,
    ai.website,
    GREATEST(iam.created_at, gd.created_at, ai.created_at) as created_at,
    GREATEST(iam.updated_at, gd.updated_at, ai.updated_at) as updated_at
FROM ip_asn_mappings iam
LEFT JOIN geographic_data gd ON iam.start_ip <<= gd.network
LEFT JOIN as_information ai ON iam.asn = ai.asn;

-- Grant permissions (adjust as needed for your security requirements)
-- GRANT USAGE ON SCHEMA internet_structure TO internet_user;
-- GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA internet_structure TO internet_user;
-- GRANT USAGE ON ALL SEQUENCES IN SCHEMA internet_structure TO internet_user;

-- Insert some initial metadata
INSERT INTO connectivity_metrics (metric_name, metric_value, data_source)
VALUES
    ('database_initialized', '{"status": "success", "timestamp": "' || CURRENT_TIMESTAMP || '"}', 'init_script')
ON CONFLICT (metric_name, calculation_date, data_source) DO NOTHING;
