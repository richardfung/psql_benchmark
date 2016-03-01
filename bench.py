import argparse
import psycopg2
import threading
import time

query_types = {"small": "SELECT * FROM small",
               "big": "SELECT * FROM big",
               "simple": "SELECT (1) as simple",
               "complex": """SELECT (1) AS "a" FROM "config_device" WHERE ("config_device"."oid" = 107374184511 AND ((("config_device"."custom_type"::text ~* '^extrahop.device.db_server$' OR ("config_device"."devclass" IS NOT NULL AND "config_device"."devclass"::text ~* '^extrahop.device.db_server$')) AND "config_device"."id" IN (SELECT U0."device_id" FROM "config_activity" U0 WHERE (U0."from_time" <= 1455216330000 AND U0."until_time" >= 1455194730000))) OR "config_device"."id" IN (SELECT U0."device_id" FROM "config_activity" U0 INNER JOIN "config_statdesc" U1 ON ( U0."stat_id" = U1."oid" ) WHERE (U0."from_time" <= 1455216330000 AND U0."until_time" >= 1455194730000 AND U1."name"::text ~* '^extrahop.device.db_server$')))) LIMIT 1"""}

def main(query_count, thread_count, query_type):
    query_count_lock = threading.Lock()
    start_semaphore = threading.Semaphore(0)

    def _run_queries(query):
        start_semaphore.acquire(True)
        conn = psycopg2.connect("dbname=test user=postgres")
        query_count_lock.acquire(True)
        while query_count_holder[0] > 0:
            query_count_holder[0] -= 1
            query_count_lock.release()
            with conn.cursor() as cursor:
                cursor.execute(query)
                res = cursor.fetchall()
            query_count_lock.acquire(True)
            # print(res)
        query_count_lock.release()
        conn.close()
    
    def _make_thread():
        return threading.Thread(target=_run_queries,
                                args=(query_types[query_type], ))

    # must use mutable variable to modify from inner function in python2
    query_count_holder = [query_count]

    threads = [_make_thread() for i in xrange(thread_count)]
    for thread in threads:
        thread.start()
    start = time.time()
    for thread in threads:
        start_semaphore.release()
    for thread in threads:
        thread.join()
    print(time.time() - start)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-q", "--queries", type=int, dest="query_count",
                        required=True)
    parser.add_argument("-t", "--threads", type=int, dest="thread_count",
                        required=True)
    parser.add_argument("-qt", "--querytype", type=str, dest="query_type",
                        required=True,
                        choices=query_types.keys())
    args = parser.parse_args()
    main(**vars(args))
