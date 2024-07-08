#!/usr/bin/python3

##############################################################################
#                                                                            #
#  This file is part of MOPSA, a Modular Open Platform for Static Analysis.  #
#                                                                            #
#  Copyright (C) 2017-2024 The MOPSA Authors.                                #
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


# Check that the source tree is ready for a release.
# Then, call GitLab to make a new release.
# opam-publish is _not_ done automatically.


import sys
import subprocess
import json
import requests
import csv
import re
import os

# Configuration
###############

gitlabURL = 'https://www.gitlab.com'
group = 'mopsa'
project = 'mopsa-analyzer'


# Command-line arguments
########################

if len(sys.argv) != 3:
    print('Usage: ./make_release.py API_TOKEN VERSION')
    sys.exit(1)

token = sys.argv[1]
version = sys.argv[2]


# Derived variables
###################

# as git tags cannot have a ~, replace with -
# opam uses tilde (~) to denote pre-releases
# semantic versioning uses hyphens (-) and starts with v
git_tag = 'v' + version.replace('~','-')

# Namespaced path encoding of project in group (used as project id in API)
projectid = group + '%2F' + project

# Project URL on GitLab
project_url = gitlabURL + '/' + group + '/' + project

# Released archive URL hosted on GitLab
archive_url = project_url + '/-/archive/' + git_tag + '/' + project + '-' + git_tag + '.tar.gz'

# GitLab API access
apiURL = gitlabURL + '/api/v4/'
headers = {'PRIVATE-TOKEN': token }


# local tree info
#################

# last_tag = subprocess.check_output(['git','describe','--abbrev=0']).decode('utf-8').strip()
branch = subprocess.check_output(['git','rev-parse','--abbrev-ref','HEAD']).decode('utf-8').strip()
commit_sha = subprocess.check_output(['git','--no-pager','log','-1','--format=%H']).decode('utf-8').strip()
commit_sha_short = subprocess.check_output(['git','--no-pager','log','-1','--format=%h']).decode('utf-8').strip()


# check some files
##################

def check_version():
    print('\nChecking the VERSION file.')
    with open('../VERSION') as f:
        l = f.read().strip().strip()
    if l == version:
        print('OK, VERSION file matches.')
    else:
        print('ERROR: the VERSION file (' + l + ') should match the version you provided (' + version + ').')
        exit(1)
    
def check_license():
    print()
    ret = subprocess.run(['./check_license.sh'])
    if ret.returncode != 0:
        print("Consider using:\n reuse annotate  --template=header.jinja2 --copyright \"Copyright (C) 2024 The MOPSA authors.\" --license LGPL-3.0-or-later")
        exit(1)

def check_changelog():
    to_find = '# ' + version
    print('\nChecking whether CHANGELOG.md has a line about version ' + version)
    with open('../CHANGELOG.md') as f:
        for l in f:
            if l.strip() == to_find:
                print('OK, the CHANGELOG.md might be up-to-date.')
                return
    print('ERROR: CHANGELOG.md has no line of the form: "' + to_find + '"')
    print('Fill-in the change-log information.')
    exit(1)

def check_authors():
    authors_md = []
    with open("../AUTHORS.md") as a:
        for l in a:
            if l.startswith("-"):
                authors_md.append(l.split("-")[1].split("<")[0].strip())
    authors_opam = []
    with open("../mopsa.opam") as a:
        l = a.readline()
        while not l.startswith("authors: ["):
            l = a.readline()
        for l in a:
            if l.startswith("]"): break
            authors_opam.append(l.strip()[1:-1])
    authors_md.sort()
    authors_opam.sort()
    if authors_md != authors_opam:
        print("ERROR: AUTHORS.md and author field of mopsa.opam do not match")
        exit(1)

# check that the local source tree is clean
###########################################

# (cached) git ls-remote
remote_cache = None
def get_remote():
    global remote_cache
    if remote_cache == None:
        remote_cache = subprocess.check_output(['git','ls-remote','-q'],stderr=subprocess.STDOUT).decode('utf-8')
    return remote_cache

# check local tree for uncommited changes
def check_no_uncommited():
    print('\nLooking for uncommited local changes.')
    out = subprocess.check_output(['git','status','--porcelain','--untracked-files=no']).decode('utf-8')
    if out == '':
        print('OK, you have no uncommited changes.')
        out = subprocess.check_output(['git','status','--porcelain']).decode('utf-8').strip()
        if out != '':
            print('However, you have untracked files:')
            print(out)
            print('Please make sure you did not forget to git add them.')
    else:
        print('\nERROR: you have uncommited changes:')
        print(out)
        print('Run git add / git commit first.')
        exit(1)

# check remote for local commit
def check_commit_sync():
    print('\nLooking for sync between remote and local.')
    if commit_sha in get_remote():
        print('OK, your commit is in the remote references.')
    else:
        print('\nERROR: your commit is not a remote reference.')
        print('Run git pull / git push first.')
        exit(1)


# git tag checking
##################

def check_no_tag():
    print('\nChecking that the version tag does not already exist.')
    ret = subprocess.run(['git','rev-parse','-q','--verify',git_tag])
    if ret.returncode == 0 or 'tags/' + git_tag + "^{}" in get_remote():
        print('\nERROR: version tag ' + git_tag + ' alread exists.')
        print('Choose a new version to release.')
        exit(1)
    else:
        print('OK, tag ' + git_tag + ' does not already exist.')


# CI checking
#############

def check_authorized(r):
    if r.status_code == 401:
        print('ERROR: Not authorized to access GitLab (code 401)')
        print('Check your access token')
        exit(1)

def check_gitlab_ci():
    print('\nChecking the commit on GitLab.')
    r = requests.get(apiURL + 'projects/' + projectid + '/repository/commits/' + commit_sha,
                     headers = headers)
    check_authorized(r)
    if r.status_code != 200:
        print('ERROR: Cannot find GitLab commit: status_code=' + str(r.status_code))
        exit(1)
    j = r.json()
    status = j.get('status')
    if status != 'success':
        print('ERROR: Commit is not successful, status is ' + str(status) + '.')
        print('Please fix the code, commit, and wait for CI on GitLab.')
        exit(1)
    pipeline = j.get('last_pipeline')
    status = pipeline.get('status') if pipeline != None else None
    if status != 'success':
        print('ERROR: Commit pipeline is not successful, status is ' + str(status))
        print('Please fix the code, commit, and wait for CI on GitLab.')
        exit(1)
    print('OK, the commit on GitLab has a successful pipeline.')



# Create GitLab release
#######################

def do_release():
    # TODO: add binary?
    print('\nMaking GitLab release.')
    data = {
        'name' : git_tag,
        'description' : '#### ' + version,
        'tag_name' : git_tag,
        'ref' : commit_sha
    }
    r = requests.post(apiURL + 'projects/' + projectid + '/releases', headers = headers, data = data)
    check_authorized(r)
    if r.status_code == 409:
        print('ERROR: Release already exists!')
        exit(1)        
    if r.status_code != 201:
        print('ERROR: Cannot create GitLab release: status_code=' + str(r.status_code))
        exit(1)
    print('\n**** Release created ****\n')
    print('Please edit release notes and assets at:')
    print(project_url + '/-/releases/' + git_tag + '/edit')


# Docker
########

def print_docker_instructions():
   print('\nYou can create and publish a Docker to the project Container Registry with:')
   print(f"docker login registry.gitlab.com -p {token}")
   # VERSIONS with ~ seem to not be accepted by Docker
   print(f"cd ../docker/analyzer && docker build -t registry.gitlab.com/{group}/{project}:{version.replace('~', '-')} .")
   print(f"docker push registry.gitlab.com/{group}/{project}:{version.replace('~', '-')}")
   print(f"docker image tag registry.gitlab.com/{group}/{project}:{version.replace('~', '-')} registry.gitlab.com/{group}/{project}:stable")
   print(f"docker push registry.gitlab.com/{group}/{project}:stable")

# OPAM 
######

def print_opam_instructions():
    print('\nYou can publish the package to opam with the following command (remove the -n after checking):')
    print('opam publish -n -v ' + version + ' ' + archive_url)


if __name__ == "__main__":
    if not os.getcwd().endswith("release"):
        print("Please run the script from the release directory")
        exit(1)
    print('You are in branch: ' + branch)
    print('at commit: ' + commit_sha_short + ' (' + commit_sha + ')')
    # print('last tag is: ' + last_tag)
    print('You are releasing version: ' + version)
    print('to: ' + project_url)
    print('The git tag for the release will be: ' + git_tag)

    check_version()
    check_license()
    check_changelog()
    check_authors()
    check_no_uncommited()
    check_commit_sync()
    check_no_tag()
    check_gitlab_ci()

    print('\nEverything seems ready for the release.')
    print('\nPlease make sure that the CHANGELOG.md, README.md and AUTHORS.md files are up to date.')
    print('\nAre you ready to perform the release?')
    print('Type Yes to proceed.')
    answer = input()
    if answer.lower() != 'yes':
        print('See you when you\'re ready!')
        exit(1)

    do_release()

    print_docker_instructions()
    print_opam_instructions()
