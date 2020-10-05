import csv
import numpy as np


def process_date(date):
    ar = date.split(' ')
    ret_arr = []
    ret_arr = ret_arr + [int(i) for i in ar[0].split('-')]
    ret_arr = ret_arr + [int(i) for i in ar[1].split(':')]
    return (ret_arr[2] - 27) * 24 * 3600 + ret_arr[3] * 3600 + ret_arr[4] * 60 + ret_arr[5]


def read(path):
    """
    This will takes in a path and then return three arrays.
    :param path: The path to the csv file
    :return: return 3 arrays:
    id_arr: the id for the rows of the array after being sorted by time
    shop_id: the original array after being sorted by shopid
    real_real_arr: the original array
    transpose_arr : the original array, flipped
    """
    with open(path, 'r') as f:
        str_read = list(csv.reader(f))
        str_read.pop(0)
        ret_arr = []
        for idx, row in enumerate(str_read):
            row[0] = int(row[0])
            row[1] = int(row[1])
            row[2] = int(row[2])
            row[3] = process_date(row[3])
            row.append(idx)
            ret_arr.append(row)
        print(ret_arr[0])

    compound_arr = ret_arr.copy()

    compound_arr = np.array(compound_arr)
    real_arr = compound_arr[:, 0:4]

    real_real_arr = compound_arr[:, 0:4]

    trans_arr = real_real_arr.copy()

    trans_arr = np.transpose(trans_arr, (1, 0))
    trans_arr = trans_arr.tolist()

    shop_id = real_arr.tolist()
    shop_id.sort(key=lambda x: x[1])

    ret_arr.sort(key=lambda x: x[3])

    arr = np.array(ret_arr)

    id_arr = arr[:, 4:5]

    id_arr = id_arr.squeeze()

    id_arr = id_arr.tolist()

    # print(id_arr[0:10])
    # print(real_arr[0:10])
    # print(trans_arr[0])

    return id_arr, shop_id, real_real_arr, trans_arr
    # return ret_arr


read('order_brush_order.csv')
