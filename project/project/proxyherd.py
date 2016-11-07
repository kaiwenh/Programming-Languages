import time
import datetime
import logging
import re
import sys
import json

from twisted.protocols.basic import LineReceiver
from twisted.web.client import getPage
from twisted.internet import reactor, protocol
from twisted.python import log
from twisted.application import service, internet


""" --- Global Definitions --- """
API_key = "AIzaSyCYDjufxDqgAdEbP5wGixpofr96iRrgETc"
API_URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"

#mapping from server to port numbers
servers = {
    "Alford"   : 11530,
    "Bolden"   : 11531,
    "Hamilton" : 11532,
    "Parker"   : 11533,
    "Welsh"    : 11534
}

#dictionary containing each server's neighbors
neighbors = {
    "Alford" :   ["Parker", "Welsh"],
    "Bolden" :   ["Parker", "Welsh"],
    "Hamilton" : ["Parker"],
    "Parker" :   ["Alford", "Bolden", "Hamilton"],
    "Welsh" :   ["Alford", "Bolden"]
}

""" --- Implementation for Proxy Herd Server --- """

class ProxyServerProtocol(LineReceiver):
    def __init__(self, factory):      
        self.factory = factory

    def connectionMade(self):      
        self.factory.numOfConnections += 1
        logging.info("Connection started. Total: {0}".format(self.factory.numOfConnections))
        

    def connectionLost(self, reasonLost):
        self.factory.numOfConnections -= 1
        log = "Connection closed. Total: {0}".format(self.factory.numOfConnections)
        

    def lineReceived(self, line):
        logging.info("[REQUEST] Recieved: {0}".format(line))

        argList = line.strip().split(" ")
        #identify command type
        if argList[0] == "AT":
            self.serveAT(line)
        elif argList[0] == 'IAMAT':
            self.serveIAMAT(line)
        elif argList[0] == "WHATSAT":
            self.serveWHATSAT(line)
        else: #invalid command
            logging.info("[REQUEST] Invalid request")
            self.transport.write(self.invalidCommand(line))
            return

    # Helper functions:
    # formatting reponse for invalid commands
    def invalidCommand(self, line):
        result = "? {0}\n".format(line)
        return result
    #flood location info to all neigboring servers
    def propagateInfo(self, response, sender):
        for name in neighbors[self.factory.serverName]:
            reactor.connectTCP('localhost', servers[name], ProxyClient(response))
            logging.info("[{0}]: [FLOOD] Sent location from {1} to {2}".format
                (self.factory.serverName, self.factory.serverName, name))
        return
    #print location info using json module
    def printInfo(self, response, limit, clientID):
        jsonData = json.loads(response)
        results = json.dumps(jsonData, indent=4)
        logging.info("[{0}]: [API] Response from Google Places API: {1}".format(self.factory.serverName ,results))
        msg = self.factory.clients[clientID]["msg"]
        writeBack = "{0}\n{1}\n\n".format(msg, results)
        self.transport.write(writeBack)
        return

    #Handler functions for the three type of commands
    def serveIAMAT(self, line):
        argList = line.strip().split(" ")

        if len(argList) != 4:
            logging.info("[{0}]: [REQUEST] Invalid number of arguments for 'IAMAT'".format(self.factory.serverName))
            self.transport.write(self.invalidCommand(line))
            return

        # bind the arguments to local variables
        commandName, clientID, position, t = argList

        clientTime = float(t)

        #invalid (nonpositive) time argument
        if t <= 0:
            logging.info("[{0}]: Time must be positive".format(self.factory.serverName))
            self.transport.write(self.invalidCommand(line))
            return

        timeDiff = time.time() - clientTime

        if timeDiff >= 0:
            response = "AT {0} +{1} {2} {3} {4}".format(self.factory.serverName,
                timeDiff, clientID, position, clientTime)
        else:
            response = "AT {0} {1} {2} {3} {4}".format(self.factory.serverName,
                timeDiff, clientID, position, clientTime)
        self.transport.write(response + "\n")

        #client not in cache: add new client
        if clientID not in self.factory.clients:
            logging.info("[{0}]: [IAMAT] New client joined: {1}".format(self.factory.serverName, clientID))
            self.factory.clients[clientID] = {"msg" : response, "time" : clientTime}
            # flood
            self.propagateInfo(response, self.factory.serverName)
        #client exists in cache, update if necessary
        else:
            if self.factory.clients[clientID]['time'] > clientTime:
                logging.info("[{0}]: [IAMAT] Duplicate Info for client: {0}".format(self.factory.serverName, clientID))
                return
            else:
                logging.info("[{0}]: [IAMAT] Updating info for existing client: {0}".format(self.factory.serverName, clientID))
                self.factory.clients[clientID] = {"msg" : response, "time" : clientTime}
                self.propagateInfo(response, self.factory.serverName)
        return

    def serveAT(self, line):
        argList = line.strip().split(" ")

        if len(argList) != 6:
            logging.info("[{0}]: Invalid number of arguments for 'AT'".format(self.factory.serverName))
            self.transport.write(self.invalidCommand(line))
            return

        # bind arguments to local variables
        commandName, serverName, timeDiff, clientID, position, clientTime = argList
        # Location info in cache is up to date: no need to update
        if clientID in self.factory.clients and clientTime <= self.factory.clients[clientID]["time"]:
            logging.info("[{0}]: [AT] Duplicate Info for client: {1}".format(self.factory.serverName, clientID))
            return
        # Location info need to be updated:
        if clientID in self.factory.clients:
            logging.info("[{0}]: [AT] Updating info for existing client: {1}".format(self.factory.serverName, clientID))
        else:
            logging.info("[{0}]: [AT] New client joined: {1}".format(self.factory.serverName, clientID))

        self.factory.clients[clientID] = {"msg" : line, "time" : clientTime}
        self.propagateInfo(line, self.factory.serverName)
        return

    def serveWHATSAT(self, line):
        argList = line.strip().split(" ")

        if len(argList) != 4:
            logging.info("[{0}]: Invalid number of arguments for 'WHATSAT'")
            self.transport.write(self.invalidCommand(line))
            return

        # bind arguments to local variables
        commandName, clientID, radius, limit = argList

        #radius and numer of items must be within bound
        if int(radius) > 50 or int(limit) > 20:
            logging.info("[{0}]: Invalid arguments for 'WHATSAT'")
            self.transport.write(self.invalidCommand(line))
            return

        # check if clientID exists
        if clientID not in self.factory.clients:
            logging.info("[{0}]: [WHATSAT] clientID '{1}' is invalid".format(self.factory.serverName, clientID))
            return

        response = self.factory.clients[clientID]["msg"]
        
        logging.info("[{0}]: [WHATSAT] Found entry in cache: {1}".format(self.factory.serverName, response))
        _, _, _, _, position, _ = response.split(" ")

        # formatting position parameters
        position = re.sub(r'[-]', ' -', position)
        position = re.sub(r'[+]', ' +', position).split()
        pos = position[0] + "," + position[1]

        # send API request and receive respnose
        request = "{0}location={1}&radius={2}&sensor=false&key={3}".format(API_URL, pos, radius, API_key)
        logging.info("[{0}]: [WHATSAT] API request created: {1}".format(request, self.factory.serverName))
        API_response = getPage(request)
        API_response.addCallback(
            lambda x:
            self.printInfo(x, int(limit), clientID))
        return


class ProxyServer(protocol.ServerFactory):
    def __init__(self, serverName):
        self.clients = {}
        self.numOfConnections = 0
        self.serverName = serverName
        self.portNum = servers[self.serverName]
        logFileName = self.serverName + ".log"
        logging.basicConfig(filename=logFileName, level=logging.DEBUG)
        logging.info("[{0}] : SERVER {1} started at PORT {2}.".format(self.serverName, self.serverName, self.portNum))
        
    def buildProtocol(self, addr):
        return ProxyServerProtocol(self)

    # report closed servers
    def stopFactory(self):
        logging.info("[{0}] : SERVER {1} on PORT {2} closed.".format(self.serverName, self.serverName, self.portNum))
        

""" --- Implementation for Proxy Herd Client --- """
class ProxyClientProtocol(LineReceiver):
    def __init__(self, factory):
        self.factory = factory

    def connectionMade(self):
        self.sendLine(self.factory.message)
        self.transport.loseConnection()


class ProxyClient(protocol.ClientFactory):
    def __init__(self, message):
        self.message = message

    def buildProtocol(self, addr):
        return ProxyClientProtocol(self)

""" --- Main Routine --- """
def main():
    if len(sys.argv) != 2:
        print "Error: invalid number of arguments."
        exit()

    factory = ProxyServer(sys.argv[1])
    reactor.listenTCP(servers[sys.argv[1]], factory)
    reactor.run()

if __name__ == '__main__':
    main()




