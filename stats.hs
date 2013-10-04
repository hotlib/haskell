{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import Data.Time.Clock (UTCTime)
import Data.List.Split


type FindLongestQueryResult = (Double, String)
findLongestQuerySelect :: Query
findLongestQuerySelect = "SELECT \
						\COALESCE(extract(epoch FROM CURRENT_TIMESTAMP-query_start),0) AS query_time_in_seconds, \
						\current_query FROM pg_stat_activity WHERE current_query NOT LIKE '<IDLE%' \
						\GROUP BY query_time_in_seconds, current_query \
						\ORDER BY query_time_in_seconds DESC \
						\LIMIT 3"

type FindLongestTransactionResult = (Only Double)
findLongestTransactionSelect :: Query
findLongestTransactionSelect = "SELECT \
							\COALESCE(max(extract(epoch FROM CURRENT_TIMESTAMP-xact_start)),0) AS transaction_time_in_seconds \
							\FROM pg_stat_activity \
							\ORDER BY transaction_time_in_seconds DESC \
							\LIMIT 3"

type TransactionStuckResult = (Bool, Maybe UTCTime, String, Maybe UTCTime, Maybe UTCTime)
transactionStuckSelect :: Query
transactionStuckSelect = "SELECT \
						\waiting, \
						\xact_start, \
						\current_query, \ 
						\query_start, \
						\backend_start \
						\FROM pg_stat_activity WHERE \
						\(current_query = '<IDLE> in transaction' OR waiting = TRUE) \
						\AND (xact_start < (now()::DATE - interval '10 minute')::TIMESTAMP)"

type LastAnalyzedAndVacuumedResult = (String, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime)
lastAnalyzedAndVacuumedSelect :: Query
lastAnalyzedAndVacuumedSelect = "SELECT \
				\schemaname || '.' || relname AS schema_table, \
				\last_vacuum, \
				\last_autovacuum, \
				\last_analyze, \
				\last_autoanalyze \
				\FROM pg_stat_all_tables \
				\WHERE NOT \
				\((last_vacuum > (now()::DATE - 14)::TIMESTAMP OR last_autovacuum > (now()::DATE - 14)::TIMESTAMP) \
	            \AND (last_analyze > (now()::DATE - 14)::TIMESTAMP OR last_autoanalyze > (now()::DATE - 14)::TIMESTAMP)) \
	  			\OR (last_autovacuum IS NULL AND last_vacuum IS NULL) \
				\OR (last_analyze IS NULL AND last_autoanalyze IS NULL)"

type UnusedIndexesResult = (String, String, Int, Int, String, String, Int)
unusedIndexesSelect :: Query
unusedIndexesSelect = "SELECT \
	\pg_index.schemaname || '.' || pg_index.relname AS schema_table, \
	\indexrelname AS index_name, \
	\pg_index.idx_scan AS index_scan_count, \
	\pg_index.idx_scan AS seq_scan_count, \
	\pg_size_pretty(pg_total_relation_size(pg_index.schemaname || '.' || pg_index.relname)) AS table_size, \
	\pg_size_pretty(pg_relation_size(quote_ident(indexrelname)::text)) AS index_size, \
	\n_tup_upd + n_tup_ins + n_tup_del as num_writes \
	\FROM pg_stat_user_indexes AS pg_index \
	\JOIN pg_indexes ON (indexrelname = indexname AND pg_index.schemaname = pg_indexes.schemaname) \
	\JOIN pg_stat_user_tables AS pg_tables ON (pg_index.relname = pg_tables.relname AND pg_index.schemaname = pg_tables.schemaname) \
	\WHERE indexdef !~* 'unique' \
	\ORDER BY index_scan_count ASC \
	\LIMIT 5"

type SharredBufferOverallResult = (Int, Int, Double)
sharredBufferOverallSelect :: Query
sharredBufferOverallSelect = "SELECT \ 
			\sum(heap_blks_read) :: BigInt as heap_read, \
            \sum(heap_blks_hit) :: BigInt as heap_hit, \
            \sum(heap_blks_hit) / (sum(heap_blks_hit) + sum(heap_blks_read)) :: Float as buffer_avg \
    		\FROM pg_statio_user_tables"



type SharredBufferTableResult = (String, Int)
sharredBufferTableSelect :: Query
sharredBufferTableSelect = "SELECT \  
				\schemaname || '.' || relname AS schema_table, \ 
				\100 * heap_blks_hit/(heap_blks_hit + heap_blks_read) as cache_hit_avg \
				\FROM pg_statio_user_tables \
				\WHERE \
				\heap_blks_read > 0 AND \
				\100 * heap_blks_hit/(heap_blks_hit + heap_blks_read) < 50 \
				\ORDER BY cache_hit_avg ASC \
				\LIMIT 5"

type LeastUsedIndexesResult = (String, Int, Int)
leastUsedIndexesSelect :: Query
leastUsedIndexesSelect = "SELECT \
				 \schemaname || '.' || relname AS schema_table, \ 
                 \100 * idx_scan / (seq_scan + idx_scan) AS index_hit_avg, \
                 \n_live_tup AS rows_in_table \
                 \FROM pg_stat_user_tables \
                 \WHERE \
                 \seq_scan + idx_scan > 0 AND \
                 \100 * idx_scan / (seq_scan + idx_scan) < 80 \
    			 \ORDER BY n_live_tup DESC, index_hit_avg DESC \
    			 \LIMIT 5" 

type TableSizeAndWritesSelectResult = (String, String, Int) 
tableSizeAndWritesSelect :: Query
tableSizeAndWritesSelect = "SELECT \
             \nspname || '.' || C.relname AS schema_table,\
			 \pg_size_pretty(pg_total_relation_size(C.oid)) AS total_size, \
		     \100 * (n_tup_ins + n_tup_del + n_tup_upd)/(idx_scan + seq_scan + n_tup_ins + n_tup_del + n_tup_upd) AS write_percentage \
	         \FROM pg_class C \
	         \LEFT JOIN pg_namespace N ON (N.oid = C.relnamespace) \
	         \LEFT JOIN pg_stat_user_tables S ON (S.relname = C.relname) \
             \WHERE nspname NOT IN ('pg_catalog', 'information_schema') \
	         \AND C.relkind <> 'i' \
	         \AND nspname !~ '^pg_toast' \
	         \AND (n_tup_ins IS NOT NULL OR n_tup_del IS NOT NULL OR n_tup_upd IS NOT NULL) \
	         \AND (seq_scan IS NOT NULL AND idx_scan IS NOT NULL) \
	         \ORDER BY write_percentage DESC \
	         \limit 5"


myConnection :: ConnectInfo
myConnection = defaultConnectInfo {connectHost = "192.168.10.144", connectPort = 5432, connectUser = "xxx", connectPassword = "xxx", connectDatabase = "xxx"}


pgRunQuery :: (FromRow r, Show r) =>  Query -> IO [r]
pgRunQuery queryString = do 
	conn <- connect myConnection
  	query_ conn queryString 



pgPrintResult :: (FromRow r, Show r) => IO [r] -> IO ()
pgPrintResult query  = do 
			result <- query
			sequence_ $ fmap print result
				

main = do
		let result  = (pgRunQuery sharredBufferTableSelect) :: IO [SharredBufferTableResult]
		let	result2 = (pgRunQuery leastUsedIndexesSelect) :: IO [LeastUsedIndexesResult]	
		let	result3 = (pgRunQuery tableSizeAndWritesSelect) :: IO [TableSizeAndWritesSelectResult]	
		let	result4 = (pgRunQuery sharredBufferOverallSelect) :: IO [SharredBufferOverallResult]	
		let	result5 = (pgRunQuery unusedIndexesSelect) :: IO [UnusedIndexesResult]	
		let	result6 = (pgRunQuery lastAnalyzedAndVacuumedSelect) :: IO [LastAnalyzedAndVacuumedResult]	
		let	result7 = (pgRunQuery transactionStuckSelect) :: IO [TransactionStuckResult]	
		let	result8 = (pgRunQuery findLongestTransactionSelect) :: IO [FindLongestTransactionResult]
		let	result9 = (pgRunQuery findLongestQuerySelect) :: IO [FindLongestQueryResult]

		pgPrintResult result 
		pgPrintResult result2
		pgPrintResult result3
		pgPrintResult result4
		pgPrintResult result5
		pgPrintResult result6
		pgPrintResult result7
		pgPrintResult result8
		pgPrintResult result9
	
