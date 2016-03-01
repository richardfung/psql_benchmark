package main

import (
	"database/sql"
	"flag"
	"fmt"
	_ "github.com/lib/pq"
	"os"
	"runtime"
	"sync"
	"time"
)

func main() {
	var queryCount int
	var threadCount int
	var queryType string
	flag.IntVar(&queryCount, "queries", 0, "number of queries")
	flag.IntVar(&queryCount, "q", 0, "number of queries")
	flag.IntVar(&threadCount, "threads", 0, "number of threads")
	flag.IntVar(&threadCount, "t", 0, "number of threads")
	flag.StringVar(&queryType, "querytype", "",
		"type of query: small, big, simple, or complex")
	flag.StringVar(&queryType, "qt", "",
		"type of query: small, big, simple, or complex")
	flag.Parse()
	if queryCount <= 0 {
		fmt.Println("queryCount must be > 0")
		os.Exit(0)
	} else if threadCount <= 0 {
		fmt.Println("threadCount must be > 0")
		os.Exit(0)
	}
	switch queryType {
	case "small", "big", "simple", "complex":
		benchmark(queryCount, threadCount, queryType)
	default:
		fmt.Println("queryType must be big, simple, small, or complex")
		os.Exit(0)
	}
}

func benchmark(queryCount int, threadCount int, queryType string) {
	var query string
	switch queryType {
	case "simple":
		query = "SELECT (1) as simple"
	case "complex":
		query = `SELECT (1) AS "a" FROM "config_device" WHERE ("config_device"."oid" = 107374184511 AND ((("config_device"."custom_type"::text ~* '^extrahop.device.db_server$' OR ("config_device"."devclass" IS NOT NULL AND "config_device"."devclass"::text ~* '^extrahop.device.db_server$')) AND "config_device"."id" IN (SELECT U0."device_id" FROM "config_activity" U0 WHERE (U0."from_time" <= 1455216330000 AND U0."until_time" >= 1455194730000))) OR "config_device"."id" IN (SELECT U0."device_id" FROM "config_activity" U0 INNER JOIN "config_statdesc" U1 ON ( U0."stat_id" = U1."oid" ) WHERE (U0."from_time" <= 1455216330000 AND U0."until_time" >= 1455194730000 AND U1."name"::text ~* '^extrahop.device.db_server$')))) LIMIT 1`
	case "small":
		query = "SELECT * FROM small"
	case "big":
		query = "SELECT * FROM big"
	}
	db, err := sql.Open("postgres", "user=postgres dbname=test")
	if err != nil {
		panic(err)
	}
	runtime.GOMAXPROCS(threadCount + 1)
	db.SetMaxIdleConns(threadCount)
	db.SetMaxOpenConns(threadCount)
	start := make(chan struct{})
	var waitGroup sync.WaitGroup
	var queryCountLock sync.Mutex
	waitGroup.Add(threadCount)
	for i := 0; i < threadCount; i++ {
		go runQuery(query, &queryCount, db, &queryCountLock, start, &waitGroup)
	}
	startTime := time.Now()
	for i := 0; i < threadCount; i++ {
		start <- struct{}{}
	}
	waitGroup.Wait()
	fmt.Println(time.Since(startTime).Seconds())
}

func runQuery(query string, queryCount *int, db *sql.DB,
	queryCountLock *sync.Mutex, start <-chan struct{},
	waitGroup *sync.WaitGroup) {
	defer func() { waitGroup.Done() }()
	<-start

	var pointers []interface{}
	queryCountLock.Lock()
	for *queryCount > 0 {
		(*queryCount)--
		// fmt.Println(*queryCount)
		queryCountLock.Unlock()
		queryResult, err := db.Query(query)
		if err != nil {
			panic(err)
		}
		columns, err := queryResult.Columns()
		if err != nil {
			panic(err)
		}
		colLen := len(columns)
		if pointers == nil {
			pointers = make([]interface{}, colLen)
		}
		var rows [][]interface{}
		for queryResult.Next() {
			lastRow := make([]interface{}, colLen)
			rows = append(rows, lastRow)
			for i, _ := range pointers {
				pointers[i] = &(lastRow[i])
			}
			queryResult.Scan(pointers...)
		}
		queryCountLock.Lock()
		// fmt.Println(rows)
	}
	queryCountLock.Unlock()
}
