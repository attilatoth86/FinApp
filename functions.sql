SELECT prosrc FROM pg_proc 
WHERE proname IN ('update_modified_column')
ORDER BY 1 DESC