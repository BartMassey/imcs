#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Unit Tests for the IMCS Client"""

from unittest import TestCase
import configparser
import re

from imcs_client.client import Client

__author__ = "Michael Lane"
__email__ = "mikelane@gmail.com"
__copyright__ = "Copyright 2017, Michael Lane"
__license__ = "MIT"

if __name__ == '__main__':
    pass


class TestClient(TestCase):
    """
    NOTE: TestClient tests require you to have a username and password stored
    in the settings.ini file.
    """
    config = configparser.ConfigParser()
    config.read_file(open('settings.ini'))
    username = config['PLAYER']['username']
    password = config['PLAYER']['password']

    def test_help(self):
        with Client(username=self.username, password=self.password) as imcs_client:
            imcs_help = '210 imcs 2.5 help\r\n help: this help\r\n quit: quit imcs\r\n me <player> <password>: log in\r\n register <player> <password>: register a new name and log in\r\n password <password>: change password\r\n list: list available games\r\n ratings: list player ratings (top 10 plus own)\r\n offer [<my-color>] [<my-time> [<opp-time>]]: offer a game with given constraints\r\n accept <game-id> [<my-color>]: accept a game with an opponent\r\n clean: cancel all my outstanding offers\r\n rerate: reset my rating to the starting default\r\n.\r\n'.strip().split(
                '\r\n')
            result = imcs_client.help()
            print(result)
            result = result.strip().split('\r\n')
            self.assertEqual(imcs_help, result)

    def test_list(self):
        list_resp_line_regex_pattern = re.compile(r'(211 \d+ available games).*')
        with Client(username=self.username, password=self.password) as imcs_client:
            result = imcs_client.list()
            print(result)
            self.assertIsNotNone(re.search(pattern=list_resp_line_regex_pattern, string=result).group())

    def test_ratings(self):
        ratings_resp_line_regex_pattern = re.compile(r'(212 ratings).*')
        with Client(username=self.username, password=self.password) as imcs_client:
            result = imcs_client.ratings()
            print(result)
            self.assertIsNotNone(re.search(pattern=ratings_resp_line_regex_pattern, string=result).group())

    def test_clean(self):
        clean_resp_line_regex_pattern = re.compile(r'204 \d+ games cleaned')
        with Client(username=self.username, password=self.password) as imcs_client:
            result = imcs_client.clean()
            print(result)
            self.assertIsNotNone(re.search(pattern=clean_resp_line_regex_pattern, string=result).group())

    def test_rerate(self):
        rerate_resp = '206 rating reset for user {}'.format(self.username)
        with Client(username=self.username, password=self.password) as imcs_client:
            result = imcs_client.rerate()
            print(result)
            self.assertEqual(rerate_resp, result)

    def test_login(self):
        login_resp = '201 hello {}'.format(self.username)
        with Client(username=self.username, password=self.password) as imcs_client:
            result = imcs_client.login(username=self.username, password=self.password)
            print(result)
            self.assertEqual(login_resp, result)

    def test_change_password(self):
        pass_resp = '203 password change for user {}'.format(self.username)
        with Client(username=self.username, password=self.password) as imcs_client:
            result = imcs_client.change_password(new_password=self.password)
            print(result)
            self.assertEqual(pass_resp, result)

    # Commented out as this requires some hands on help to test. The offer() method is blocking
    # and waits on a game to be accepted. So the easiest way to test this is to telnet into the
    # game server (uncomment this test, of course) and accept a game (which this opponent will
    # quickly resign).
    #
    # def test_offer(self):
    #     test_params = [{'my_color': 'W', 'opp_time': 600}]
    #     response_pattern = re.compile(r'(1 [B|W]).*')
    #     for params in test_params:
    #         with Client(username=self.username, password=self.password) as imcs_client:
    #             result = imcs_client.offer(**params)
    #             print(result)
    #             self.assertIsNotNone(re.search(pattern=response_pattern, string=result).group())
    #             imcs_client.resign()

    def test_accept(self):
        response_pattern = re.compile(r'(1 [B|W]).*')
        with Client(username=self.username, password=self.password) as imcs_client:
            games = imcs_client.list()
            print(games)
            games = games.strip().split('\r\n ')
            game_number, opp_name, opp_color, my_time_str, opp_time_str, opp_rating, listing_type = games[-2].split()
            response = imcs_client.accept(game_number=game_number, color='W')
            print(response)
            self.assertIsNotNone(re.search(pattern=response_pattern, string=response).group())
            new_state = imcs_client.make_move('d2-d3')
            print(new_state)
            imcs_client.resign()
