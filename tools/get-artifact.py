#!/usr/bin/python3

##############################################################################
#                                                                            #
#  This file is part of MOPSA, a Modular Open Platform for Static Analysis.  #
#                                                                            #
#  Copyright (C) 2022- The MOPSA Project.                                    #
#                                                                            #
#  This program is free software: you can redistribute it and/or modify      #
#  it under the terms of the GNU Lesser General Public License as published  #
#  by the Free Software Foundation, either version 3 of the License, or      #
#  (at your option) any later version.                                       #
#                                                                            #
#  This program is distributed in the hope that it will be useful,           #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#  GNU Lesser General Public License for more details.                       #
#                                                                            #
#  You should have received a copy of the GNU Lesser General Public License  #
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
#                                                                            #
##############################################################################

import sys
import json
import requests
import csv
import re
import argparse


# Command-line options
######################

parser = argparse.ArgumentParser(description='Download artifacts of a GitLab CI job.')

parser.add_argument('project',
                    help="Name of the GitLab project")

parser.add_argument('job',
                    help="Name of the CI job")

parser.add_argument('--group',
                    help="Name of the GitLab group",
                    default="mopsa")

parser.add_argument('--token',
                    help="Authorization token",
                    required=True)

parser.add_argument('--output',
                    help="Path to output artifact file",
                    default="artifacts.zip")

args = parser.parse_args()

# Configuration
###############


gitlabURL = 'https://www.gitlab.com'

# Access to GitLab API
apiURL = gitlabURL + '/api/v4/'

# Personal access token, needed to access GitLab API
token = args.token

headers = {'PRIVATE-TOKEN': token }

# project id, or URL-encoded path (e.g. <group>%2F<project>)
projectid = args.group + '%2F' + args.project

# job to look for
job_name = args.job

# output filename
output_file = args.output



# GitLab API
############


def get_project_pipelines(projectid):
    r = requests.get(apiURL + 'projects/' + str(projectid) + '/pipelines',
                     headers = headers,
                     params = { 'order_by': 'updated_at', 'per_page': '100' })
    if r.status_code != 200:
        raise Exception('cannot find pipelines for project ' + projectid)
    return r.json()

def get_last_successful_pipeline(projectid,pipelines):
    for p in pipelines:
        if p.get('status') == 'success' and p.get("ref") == 'master' and p.get("source") in ['schedule', 'web']:
            return p
    raise Exception('cannot find the last successful pipeline for project ' + projectid)

def get_pipeline_jobs(projectid,pipelineid):
    r = requests.get(apiURL + 'projects/' + str(projectid) + '/pipelines/' + str(pipelineid) + '/jobs',
                     headers = headers,
                     params = { 'per_page': '100' })
    if r.status_code != 200:
        raise Exception('cannot find jobs for project ' + projectid + ' pipeline ' + str(pipelineid))
    return r.json()

def get_pipeline_job_by_name(projectid,pipelineid,name):
    for j in get_pipeline_jobs(projectid,pipelineid):
        if j.get('name') == name:
            return j

def get_job_zipped_artifacts(projectid,jobid):
    r = requests.get(apiURL + 'projects/' + str(projectid) + '/jobs/' + str(jobid) + '/artifacts',
                     headers = headers)
    if r.status_code != 200:
        raise Exception('cannot find artifacts for project ' + projectid + ' job ' + str(jobid))
    return r

def beautify_date(date):
    d = re.match("([0-9]+)-([0-9]+)-([0-9]+)", date)
    return d.group(3) + "/" + d.group(2) + "/" + d.group(1)


if __name__ == "__main__":
    pipelines = get_project_pipelines(projectid)
    pipeline = get_last_successful_pipeline(projectid,pipelines)
    print('pipeline date: ' + beautify_date(pipeline.get('updated_at')))
    print('pipeline id: ' + str(pipeline.get('id')))
    print('pipeline status: ' + pipeline.get('status'))
    job = get_pipeline_job_by_name(projectid,pipeline.get('id'),job_name)
    print('job name: ' + job.get('name'))
    print('job id: ' + str(job.get('id')))
    print('job date: ' + beautify_date(job.get('finished_at')))
    print('job status: ' + job.get('status'))
    zip = get_job_zipped_artifacts(projectid,job.get('id'))
    with open(output_file, 'wb') as f:
        f.write(zip.content)
    print('zipped artifacts writen to ' + output_file)
