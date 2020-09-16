#!/usr/bin/env python3

import os
import os.path
import subprocess
import sys
import time
import json
import urllib
import urllib.request
import re
import csv
import io


def p(str):
  print(str, flush=True)


def logfile(dir, title):
  return "logs/" + dir + "_" + title + ".log"


def run(dir, title, *args, **kwargs):
  logfilename = logfile(dir, title)
  with open(logfilename, "w") as file:
    result = subprocess.run(*args,
                            stdout=file,
                            stderr=subprocess.STDOUT,
                            **kwargs)
    if result.returncode != 0:
      raise Exception(
          f"Failure running {args} - see {logfilename} for details")
    return result


#def run_and_save(dir, title, *args, **kwargs):
#  with open(logfile(dir, title), "w") as file:
#    result = subprocess.check_output(*args, stderr=subprocess.STDOUT, **kwargs)
#    result = str(result)
#    file.write(result)
#    return result


def TODO(str):
  p("TODO: " + str + "\n\n\n\n")
  sys.exit(-1)


def build(dir):
  p("  Building")
  run(dir, "build", "./build.sh", cwd=dir)


def install(dir):
  p("  Fetching dependencies")
  run(dir, "install", "./install.sh", cwd=dir)


def start_server(dir):
  p("  Starting server")
  file = open(logfile(dir, "server"), "w")
  handle = subprocess.Popen("./exe",
                            cwd=dir,
                            stderr=subprocess.STDOUT,
                            stdout=file)
  time.sleep(2)
  return handle


def stop_server(dir, handle):
  p("  Stopping server")
  handle.kill()


def measure(dir, url):
  p("  Measuring")
  run(dir, "measure", ["hey", "-n", "10000", "-c", "50", "-o", "csv", url])


def report(dir, url):
  fail_count = 0
  total_response_time = 0.0
  results = []

  with open(logfile(dir, "measure"), "r") as file:
    # skip the header
    iter = csv.reader(file)
    next(iter)

    for line in iter:
      response_time = float(line[0])
      status_code = int(line[6])
      if status_code != 200: fail_count = fail_count + 1
      total_response_time = response_time + total_response_time
      results.append(response_time)

  avg = total_response_time / len(results)
  sorted_list = sorted(results)
  print(f"    Response count:  {len(sorted_list):6d}")
  print(f"    Failed requests: {fail_count:6d}")
  print(f"    Avg:             {avg:6.3f}")
  print(
      f"    95th:            {sorted_list[int(len(sorted_list)*0.95 + 1)]:6.3f}"
  )
  print(f"    Fastest:         {sorted_list[0]:6.3f}")
  print(f"    Slowest:         {sorted_list[-1]:6.3f}")


def warmup(dir, url):
  run(dir, "warmup", ["hey", "-n", "1000", "-c", "50", url])


def fizzbuzz():
  result = []
  for i in range(1, 101):
    if i % 15 == 0:
      result.append("fizzbuzz")
    elif i % 5 == 0:
      result.append("buzz")
    elif i % 3 == 0:
      result.append("fizz")
    else:
      result.append(str(i))
  return result


valid_response = fizzbuzz()


def test_output(dir, url):
  p("  Testing output")
  response = None
  body = None
  answer = None
  try:
    response = urllib.request.urlopen(url)
    body = response.read()
    answer = json.loads(body)
  except:
    p("Failed to read test output")
    p(f"Response {response}")
    p(f"Body {body}")
    p(f"Answer {answer}")

  return answer == valid_response


def get_url(dir):
  with open(dir + "/port", "r") as myfile:
    lines = myfile.readlines()
    port = "".join(lines)
    port = port.strip()
  return "http://localhost:" + port


def benchmark(dir):
  p("Benchmarking " + dir)
  install(dir)
  build(dir)
  url = get_url(dir)
  handle = start_server(dir)
  try:
    if not test_output(dir, url):
      p("  Failed test")
    else:
      warmup(dir, url)
      measure(dir, url)
      report(dir, url)
  finally:
    stop_server(dir, handle)


if len(sys.argv) > 1:
  p("Benchmarking just " + sys.argv[1])
  benchmark(sys.argv[1])
else:
  p("Starting entire benchmark")
  for f in os.listdir():
    if (os.path.isdir(f) and (not f.startswith("."))
        and (os.path.exists(f + "/build.sh"))):
      benchmark(f)

print("")
