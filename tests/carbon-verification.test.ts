import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const wallet1 = accounts.get("wallet_1")!;
const wallet2 = accounts.get("wallet_2")!;
const deployer = accounts.get("deployer")!; 


describe("carbon platform", () => {
    describe("carbon offset verification", () => {
        it("successfully registers and verifies offset", () => {
            const registerCall = simnet.callPublicFn(
                "carbon-verification",
                "register-offset",
                [Cl.uint(100)],
                wallet1
            );
            expect(registerCall.result).toBeOk(Cl.uint(0));

            const verifyCall = simnet.callPublicFn(
                "carbon-verification",
                "verify-offset",
                [Cl.uint(0)],
                deployer
            );
            expect(verifyCall.result).toBeOk(Cl.bool(true));
        });

        it("fails to register offset with zero amount", () => {
          const registerCall = simnet.callPublicFn(
              "carbon-verification",
              "register-offset",
              [Cl.uint(0)],
              wallet1
          );
          expect(registerCall.result).toBeErr(Cl.uint(101));
      });


      it("successfully verifies an offset", () => {
        // First register an offset
        simnet.callPublicFn(
            "carbon-verification",
            "register-offset",
            [Cl.uint(100)],
            deployer
        );

        // Then verify it
        const verifyCall = simnet.callPublicFn(
            "carbon-verification",
            "verify-offset",
            [Cl.uint(0)],
            deployer
        );
        expect(verifyCall.result).toBeOk(Cl.bool(true));
    });

    it("retrieves correct offset information", () => {
      // First register an offset
      simnet.callPublicFn(
          "carbon-verification",
          "register-offset",
          [Cl.uint(100)],
          wallet1
      );

      // Then get the offset info
      const getOffsetCall = simnet.callReadOnlyFn(
          "carbon-verification",
          "get-offset",
          [Cl.uint(0)],
          wallet1
      );
      
      const result = getOffsetCall.result;
      expect(result).toHaveProperty('value.data.amount.value', 100n);
      expect(result).toHaveProperty('value.data.verified.type', 4); // boolean type
      expect(result).toHaveProperty('value.data.owner.type', 5); // principal type
  });
});

    });

    describe("carbon token operations", () => {
        it("mints tokens and checks balance", () => {
            const mintCall = simnet.callPublicFn(
                "carbon-verification",
                "mint",
                [Cl.uint(1000), Cl.principal(wallet1)],
                wallet1
            );
            expect(mintCall.result).toBeOk(Cl.bool(true));

            const balanceCall = simnet.callReadOnlyFn(
                "carbon-verification",
                "get-balance",
                [Cl.principal(wallet1)],
                wallet1
            );
            expect(balanceCall.result).toBeUint(1000);
        });
    });

    describe("marketplace operations", () => {
        it("creates and purchases listing", () => {
            // First mint tokens
            simnet.callPublicFn(
                "carbon-verification",
                "mint",
                [Cl.uint(1000), Cl.principal(wallet1)],
                wallet1
            );

            // Create listing
            const listingCall = simnet.callPublicFn(
                "carbon-verification",
                "create-listing",
                [Cl.uint(100), Cl.uint(50)],
                wallet1
            );
            expect(listingCall.result).toBeOk(Cl.uint(0));

            // Purchase listing
            const purchaseCall = simnet.callPublicFn(
                "carbon-verification",
                "purchase-listing",
                [Cl.uint(0)],
                wallet2
            );
            expect(purchaseCall.result).toBeOk(Cl.bool(true));
        });
    });
