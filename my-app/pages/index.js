"use client";

import { useState, useEffect } from "react";
import {
  AppConfig,
  UserSession,
  showConnect,
  openContractCall,
} from "@stacks/connect";
import { StacksTestnet } from "@stacks/network";
import {
  tupleCV,
  uintCV,
  bufferCV,
  principalCV,
  listCV,
} from "@stacks/transactions";
import { hexToBytes } from "@stacks/common";
import { Transaction } from "bitcoinjs-lib";

export default function Home() {
  const [userData, setUserData] = useState({});
  const [blockDetails, setBlockDetails] = useState({});

  const appConfig = new AppConfig();
  const userSession = new UserSession({ appConfig });

  // Effect hook to check and see if the tx has been confirmed using blockstream API
  useEffect(() => {
    const intervalId = setInterval(() => {
      const txid = JSON.parse(localStorage.getItem("txid"));
      console.log("txid: ", txid);
      if (txid) {
        fetch(`https://blockstream.info/testnet/api/tx/${txid}/status`)
          .then((response) => response.json())
          .then(async (status) => {
            // set txStatus in localStorage if it is confirmed, otherwise we want to leave it pending
            if (status.confirmed) {
              localStorage.setItem("txStatus", "confirmed");
              // set the block details
              const blockDetails = {
                block_height: status.block_height,
                block_hash: status.block_hash,
              };
              setBlockDetails(blockDetails);
              localStorage.setItem(
                "blockDetails",
                JSON.stringify(blockDetails)
              );
              // fetch and set the tx raw
              const rawResponse = await fetch(
                `https://blockstream.info/testnet/api/tx/${txid}/hex`
              );
              const txRaw = await rawResponse.text();
              console.log("Raw TX: ", txRaw);
              localStorage.setItem("txRaw", txRaw);
              // fetch and set the merkle proof
              const proofResponse = await fetch(
                `https://blockstream.info/testnet/api/tx/${txid}/merkle-proof`
              );
              const txMerkleProof = await proofResponse.json();
              localStorage.setItem(
                "txMerkleProof",
                JSON.stringify(txMerkleProof)
              );
              clearInterval(intervalId);
            }
          })
          .catch((err) => console.error(err));
      }
    }, 10000);
    return () => clearInterval(intervalId); // Clean up on component unmount
  }, []);

  useEffect(() => {
    if (userSession.isSignInPending()) {
      userSession.handlePendingSignIn().then((userData) => {
        setUserData(userData);
      });
    } else if (userSession.isUserSignedIn()) {
      setUserData(userSession.loadUserData());
    }
  }, []);

  const connectWallet = () => {
    showConnect({
      userSession,
      network: StacksTestnet,
      appDetails: {
        name: "Frens",
        icon: "https://freesvg.org/img/bitcoin.png",
      },
      onFinish: () => {
        window.location.reload();
      },
      onCancel: () => {
        // handle if user closed connection prompt
      },
    });
  };

  const disconnectWallet = () => {
    userSession.signUserOut(window.location.origin);
    setUserData({});
    localStorage.clear();
    console.log("CLeared!");
    console.log(localStorage);
  };

  // This function sends a Bitcoin transaction and stores the raw transaction and merkle proof in localStorage
  const reserveFren = async () => {
    const resp = await window.btc?.request("sendTransfer", {
      address: "tb1qeeha0809zr9andxruvgg6fy9glt70qqeysrk8h",
      amount: "100",
    });

    // Storing txid in local storage
    if (typeof window !== "undefined") {
      localStorage.setItem("txid", JSON.stringify(resp.result.txid));
      console.log("txid: ", JSON.stringify(resp.result.txid));
    }

    localStorage.setItem("txStatus", "pending");
  };

  const removeWitnessData = (txHex) => {

    const tx = Transaction.fromHex(txHex);

    // Create a new empty transaction
    const newTx = new Transaction();

    // Copy version from original transaction
    newTx.version = tx.version;

    // Copy inputs from original transaction
    tx.ins.forEach((input) => {
      newTx.addInput(input.hash, input.index);
    });

    // Copy outputs from original transaction
    tx.outs.forEach((output) => {
      newTx.addOutput(output.script, output.value);
    });

    // Copy locktime from original transaction
    newTx.locktime = tx.locktime;
    
    return newTx.toHex();
  };

  const publicKeyToP2WPKHAddress = (publicKey, network) => {
    const p2wpkh = payments.p2wpkh({
      pubkey: publicKey,
      network: network,
    });

    return p2wpkh.address;
  };

  const verifyCorrectSender = () => {
    const txRaw = localStorage.getItem("txRaw");
    const tx = Transaction.fromHex(txRaw);
    const witness = tx.ins[0].witness;
    const publicKey = witness[witness.length - 1]; // The public key is the last item in the witness stack

    const address = publicKeyToP2WPKHAddress(publicKey, networks.testnet);
    if (address === userData.profile.btcAddress.p2wpkh.testnet) {
      return true;
    }
    return false;
  };

  // This function retrieves raw transaction and merkle proof from localStorage and calls the mint Clarity function
  const mintFrens = async () => {
    // Retrieving rawTx and merkleProof from local storage
    let txRaw = "";
    let txMerkleProof = "";

    if (typeof window !== "undefined") {
      txRaw = removeWitnessData(localStorage.getItem("txRaw"));
      txMerkleProof = JSON.parse(localStorage.getItem("txMerkleProof"));
    }

    // First we need to verify that the sender of this transaction is the same as the user that is signed in.
    // This is done on the UI and not on the contract level as the current implementation of the bitcoin-clarity
    // library doesn't support taproot and segwit yet. It is still WIP.
    // This would be changed after the implementation is complete.
    if (!verifyCorrectSender()) {
      console.log("wrong sender");
      return false;
    }

    const blockHeight = blockDetails.block_height;

    // Fetch the block hash
    const blockHashResponse = await fetch(
      `https://blockstream.info/testnet/api/block-height/${blockHeight}`
    );
    const blockHash = await blockHashResponse.text();

    // Fetch the block header
    const blockHeaderResponse = await fetch(
      `https://blockstream.info/testnet/api/block/${blockHash}/header`
    );
    const blockHeaderHex = await blockHeaderResponse.text();

    const txIndex = txMerkleProof.pos;
    const hashes = txMerkleProof.merkle.map(
      (hash) => bufferCV(hexToBytes(hash).reverse()) // lib needs reversed hashes
    ); // Convert each hash to BufferCV and reverse it

    const functionArgs = [
      principalCV(userData.profile.stxAddress.testnet),
      uintCV(blockHeight),
      bufferCV(Buffer.from(txRaw, "hex")),
      bufferCV(Buffer.from(blockHeaderHex, "hex")),
      tupleCV({
        "tx-index": uintCV(txIndex),
        hashes: listCV(hashes),
        "tree-depth": uintCV(txMerkleProof.merkle.length),
      }),
    ];

    const contractAddress = "STH0Q890NVFSW78EY65VP69SXJRZFWY3F218HXJ4"; 
 
    const contractName = "frens-mint-v8";

    const functionName = "mint";

    const options = {
      contractAddress,
      contractName,
      functionName,
      functionArgs,
      appDetails: {
        name: "Frens",
        icon: "https://freesvg.org/img/bitcoin.png",
      },
      onFinish: (data) => {
        console.log(data);
      },
    };

    await openContractCall(options);
  };

  const getButtonState = () => {
    if (localStorage.getItem("txid")) {
      if (localStorage.getItem("txStatus") == "pending") {
        return {
          text: "Transaction Pending",
          onClick: null,
          disabled: true,
          instructions: "Step 2: Wait for your transaction to confirm",
        };
      } else if (localStorage.getItem("txStatus") == "confirmed") {
        return {
          text: "Mint Your Fren",
          onClick: mintFrens,
          disabled: false,
          instructions: "Step 3: Mint your Fren",
        };
      }
    }
    return {
      text: "Reserve Your Fren",
      onClick: reserveFren,
      disabled: false,
      instructions: "Step 1: Reserve your Fren by sending 100 sats",
    };
  };

  return (
    <main className="flex flex-col items-center bg-black justify-center min-h-screen p-24">
      <h1 className="text-6xl font-bold text-center text-white py-10">Frens</h1>
      <p className="text-3xl antialiased italic font-light text-center text-red-300 py-10">you can ONLY mint your NFT if you send exactly 100 sats!</p>
      {!userData.profile ? (
        <button
          className="px-4 py-2 mt-4 text-lg font-bold text-white bg-indigo-600 rounded hover:bg-indigo-500"
          onClick={connectWallet}
        >
          Connect Your Wallet
        </button>
      ) : (
        <>
          {(() => {
            const buttonState = getButtonState();
            return (
              <>
                <p className="text-xl text-white py-2.5">{buttonState.instructions}</p>
                <button
                  className="px-4 py-2 mt-4 text-lg font-bold text-white bg-indigo-600 rounded hover:bg-indigo-500"
                  onClick={buttonState.onClick}
                  disabled={buttonState.disabled}
                >
                  {buttonState.text}
                  {buttonState.disabled && <span className="spinner"></span>}
                </button>
              </>
            );
          })()}
          <button
            className="px-4 py-2 mt-4 text-lg font-bold text-indigo-600 bg-white rounded hover:bg-indigo-500"
            onClick={disconnectWallet}
          >
            Disconnect Wallet
          </button>
        </>
      )}
    </main>
  );
}