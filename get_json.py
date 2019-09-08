import urllib.request as rq
import argparse
import json
from time import sleep


def download(url):
    print("Fetching from: " + url)
    response = rq.urlopen(url)
    data = json.load(response)
    if data['status'] != 'success':
        print("Failed to download, retrying in one second...")
        sleep(1.0)
        download(url)
    else:
        return data['data']


def courses_json(args):
    acc = []
    for sub in args.subjects:
        url = "https://classes.cornell.edu/api/2.0/search/classes.json?" + \
            "roster=" + args.semester + \
            "&subject=" + sub
        courses = download(url)
        print("Classes for " + sub + " downloaded successfully.")
        acc.extend(courses['classes'])
    return acc


def subjects_json(args):
    url = "https://classes.cornell.edu/api/2.0/config/subjects.json?" + \
        "roster=" + args.semester
    subjects = download(url)
    return subjects['subjects']


parser = argparse.ArgumentParser(
    prog="python get_json.py",
    description="Fetches a JSON from the Cornell roster.")
parser.add_argument('-o', '--output', metavar='FILE', default=None,
                    help="specify a file to output to.")

subparser = parser.add_subparsers()
cour_parser = subparser.add_parser('courses', help="Get json that list courses \
                                        for the given subject(s)")
cour_parser.add_argument('-s', '--semester', metavar='SEM', required=True,
                         help="specify a semester, first two letters is the season \
                    [SP, SU, FA, WI], and last two is the year.")
cour_parser.add_argument('subjects', nargs='+', metavar='sub',
                         help="subject from the roster to include")
cour_parser.set_defaults(func=courses_json)

subj_parser = subparser.add_parser('subjects', help="Get json that list available \
                                                     subjects for the semester.")
subj_parser.add_argument('-s', '--semester', metavar='SEM', required=True,
                         help="specify a semester, first two letters is the season \
                    [SP, SU, FA, WI], and last two is the year.")
subj_parser.set_defaults(func=subjects_json)


args = parser.parse_args()

if args.output is None:
    s = json.dumps(args.func(args), indent=2)
    print(s)
else:
    with open(args.output, 'w') as out:
        json.dump(args.func(args), out, indent=2)
