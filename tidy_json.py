import argparse
import json

parser = argparse.ArgumentParser(
    description="Removes fields from a json.")
parser.add_argument('-i', '--input', metavar='IN', default='raw_data.json',
                    help="specify a file to clean up")
parser.add_argument('-o', '--output', metavar='FILE', default='data.json',
                    help="specify a file to output to.")
parser.add_argument('-c', '--config', metavar='CONFIG', default='config.json',
                    help="specify a config file, will make one if it doesn't exist")
args = parser.parse_args()


def replace_values(d):
    acc_dict = dict()
    for k, v in d.items():
        if isinstance(v, dict):
            acc_dict[k] = replace_values(v)
        elif isinstance(v, list):
            if not (len(v) == 0 or isinstance(v[0], list) or isinstance(v[0], dict)):
                acc_dict[k] = True
            else:
                acc_dict[k] = dict()
                for e in v:
                    acc_dict[k] = {**acc_dict[k], **replace_values(e)}
        else:
            acc_dict[k] = True
    return acc_dict


def remove_cfg_keys(h, cfg):
    for k, v in cfg.items():
        if v == False:
            del h[k]
        if isinstance(v, dict):
            if isinstance(h[k], list):
                for d_k in h[k]:
                    remove_cfg_keys(d_k, cfg[k])
            else:
                remove_cfg_keys(h[k], cfg[k])


with open(args.input, 'r') as in_file:
    data = json.load(in_file)

try:
    cfg_file = open(args.config, 'x')
except FileExistsError:
    print("Found config file: " + args.config)
    with open(args.config) as cfg_file:
        cfg = json.load(cfg_file)
    for course in data:
        remove_cfg_keys(course, cfg)
else:
    print("No config found, making: " + args.config)
    new_cfg = {'_comment': "Change any value to false if the field is unnecessary."}
    for course in data:
        new_cfg = {**new_cfg, **replace_values(course)}
    json.dump(new_cfg, cfg_file, indent=2)

with open(args.output, 'w') as out:
    json.dump(data, out, indent=2)
