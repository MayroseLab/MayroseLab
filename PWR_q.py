import os
import re
import collections
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('--keep_tmp', action="store_true", default=False, help="Do not delete tmp files at the end" )
parser.add_argument('--queues_list', default='all', help="comma-separated list of queues to be displayed (all by default)")
args = parser.parse_args()

DEBUG_FLAG = 0
REM_FILES = 0

def get_total_cpus(nodes_list):
    Total_Avail=0
    cmd_all_cpus="pbsnodes -aSj | grep '"
    for node_id in nodes_list:
        node_num = node_id.replace("compute-","")
        cmd_all_cpus+="%s\|"%node_id
    cmd_all_cpus+="'" + " > CPU_data.txt"
    #print(cmd_all_cpus)
    os.system(cmd_all_cpus)
    cpu_dict=dict()
    with open('CPU_data.txt','r') as cpu_f:
        for line in cpu_f:
            if 'ngpus' in line or 'f/t' in line or '---------------' in line:
                continue
            line_split = line.split()#
            node_name = line_split[0]
            cpu_data = line_split[6]
            cpu_used = cpu_data.split('/')[0]
            cpu_avail = cpu_data.split('/')[1]
            if node_name in nodes_list:
                cpu_dict[node_name]=cpu_data
                Total_Avail += int(cpu_avail)
                #cpu_dict[node_name]=cpu_avail
                #Total_Avail += int(cpu_avail)

    if REM_FILES == 1: os.remove('CPU_data.txt')
    return Total_Avail,cpu_dict

#-----------------------------------------------------------------------------------------------------------------

def count_occurrences(word, sentence):
    count_word= sentence.count(word)
    #if count_word > 1: print(count_word)
    return count_word

def check_CPUs(line):
    cpu_num_dict=dict()
    line_split = line.split()
    spl_cpus = line_split[11].split('+')
    #print(spl_cpus)
    for item in spl_cpus:
        if '/0*' in item:
            item_split = item.split('/0*')
            node = item_split[0]
            cpu_num = item_split[1]
            cpu_num_dict[node]=int(cpu_num)
        else:
            item_split = item.split('/')
            node = item_split[0]
            cpu_num_dict[node] = 1            #compute-0-246/26   compute-0-247/22   compute-0-248/28  compute-0-249/26
    if DEBUG_FLAG == 1: print(cpu_num_dict)
    return cpu_num_dict


nodes_list = ['compute-0-285', 'compute-0-262', 'compute-0-281', 'compute-0-246', 'compute-0-278', 'compute-0-273', 
              'compute-0-283', 'compute-0-302', 'compute-0-277', 'compute-0-276', 'compute-0-260', 'compute-0-259', 
              'compute-0-247', 'compute-0-275', 'compute-0-269', 'compute-0-301', 'compute-0-249', 'compute-0-282', 
              'compute-0-279', 'compute-0-274', 'compute-0-261', 'compute-0-270']
if args.queues_list == "all":
    queue_list = ['itaym','itaym1','itaym2','itaym3','lifesciweb','itaymaa','itaymr','itaym_anat','itaym_others',
                    'itay_25_1','itay_25_2','itay_25_3','itay_25_4']
else:
    queue_list = args.queues_list.split(',')

#create qstat according to queue list:
qstat_str = "qstat "
for que in queue_list:
    qstat_str+=("%s " %que)
qstat_str+="> log_status.txt"
os.system(qstat_str)

#Collect data according to queue:
dict_queue_data = dict()
with open('log_status.txt','r') as f_status:
    for line in f_status:
        if 'Job id' in line or '----------------' in line:
            continue
        data_list = []
        line_split = line.split()
        user_name = line_split[2]
        queue = line_split[5]
        status = line_split[4]
        data_list.append(user_name)
        data_list.append(status)
        data_list.append(queue)
        #dict_queue_data[queue].append(data_list)#
        if queue in dict_queue_data.keys():
            dict_queue_data[queue].append(data_list)
        else:
            dict_queue_data[queue]=[data_list]

if REM_FILES == 1: os.remove('log_status.txt')

#print(dict_queue_data['itaym1'])
for key in dict_queue_data.keys():  # For each queue
    count_user_Qjobs = dict()
    count_user_Rjobs = dict()
    count_user_Sjobs = dict()   #suspended
    for item in dict_queue_data[key]:
        user = item[0]
        if 'Q' == item[1]:
            if user in count_user_Qjobs.keys():
                count_user_Qjobs[user]+=1
            else:
                count_user_Qjobs[user]= 1
        if 'R' in item[1]:
            if user in count_user_Rjobs.keys():
                count_user_Rjobs[user]+=1
            else:
                count_user_Rjobs[user] = 1
        if 'S' in item[1]:
            if user in count_user_Sjobs.keys():
                count_user_Sjobs[user]+=1
            else:
                count_user_Sjobs[user] = 1
    print("---------------------------------------")
    print("Queue : " + key)
    for user in count_user_Rjobs.keys():
        print("R -> %s %d jobs" % (user, count_user_Rjobs[user]))
    for user in count_user_Qjobs.keys():
        print("Q -> %s %d jobs" %(user,count_user_Qjobs[user]))
    for user in count_user_Sjobs.keys():
        print("S -> %s %d jobs" %(user,count_user_Sjobs[user]))

#-------------------------------------------------------------------------------------------------------------
#create qstat according to queue list:
qstat_more_str = "qstat -n -1 -w "
for que in queue_list:
    qstat_more_str+=("%s " %que)
qstat_more_str+="> log_status_more.txt"
os.system(qstat_more_str)

with open('log_status_more.txt','r') as f_status:
    user_nodes_dict = dict()
    user_cpus_dict = dict()
    for line in f_status:
        if ' R ' not in line:
            continue
        line_split = line.split()
        user_name = line_split[1]
        queue = line_split[2]
        m1 = re.search('compute-0-\d\d\d', line)
        node_cpu_dict = check_CPUs(line)
        if m1:
            node_id = m1.group(0)
            if DEBUG_FLAG == 1: print(node_id)
        for node_key in node_cpu_dict.keys():
            for meme in range(node_cpu_dict[node_key]):
                if user_name in user_nodes_dict.keys():
                    user_nodes_dict[user_name].append(node_key)
                else:
                    user_nodes_dict[user_name]=[node_key]
if REM_FILES == 1: os.remove('log_status_more.txt')

#print(user_nodes_dict['halabikeren'])
print("------------------------------------------------------------------------------")
print("-------------------------           CPUs usage         -----------------------")
print("------------------------------------------------------------------------------")
total_CPU,cpu_dict = get_total_cpus(nodes_list)
header = '{:22}'.format(' ')

user_list = sorted(user_nodes_dict.keys())
for user in sorted(user_nodes_dict.keys()):
    header+= '{:12}'.format(user)
print(header)
Total_CPUs = 0
for node_id in nodes_list:
    #print (node_id + '(%s)' %cpu_dict[node_id])

    data_line = '{:22}'.format(node_id + '(%s)' %cpu_dict[node_id])
    node_used_cpus=0
    for user in user_list:
        total = 0
        user_str = "{:<18}".format(user)
        #data_line += user_str
        count_items = collections.Counter(user_nodes_dict[user])
        total += count_items[node_id]
        Total_CPUs+=total
        #print("%d - total"%total)
        data_line += ('|' + '{:12}'.format(str(total)))
    print(data_line)

print("------------------------------------------------------------------------------")
print("Total CPUs used : %d out of %d" %(Total_CPUs,total_CPU))
print("------------------------------------------------------------------------------")

if not args.keep_tmp:
    os.remove('log_status.txt')
    os.remove('log_status_more.txt')
    os.remove('CPU_data.txt')
