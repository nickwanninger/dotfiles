// ~/.pi/agent/extensions/emacs-bridge.ts
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

export default function (pi: ExtensionAPI) {

  pi.registerFlag("emacs", {
    description: "emacs extensions",
    type: "string",
    default: "",
  });


  pi.on("session_start", async (_event, ctx) => {
    // console.log("emacs flag", pi.getFlag("emacs"))
  });



  pi.registerTool({
    name: "emacs_action",
    label: "Emacs Action",
    description: "Ask the Emacs RPC host to perform an editor-aware action",
    parameters: Type.Object({
      action: Type.String({
        description: "Action to perform, e.g. current_buffer_path, selected_text, eval_elisp"
      }),
      payload: Type.Optional(
        Type.String({ description: "Optional text payload for the action" })
      )
    }),
    async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
      const request = JSON.stringify({
        action: params.action,
        payload: params.payload ?? ""
      });

      const value = await ctx.ui.input(
        "Emacs bridge request",
        `Resolve this request in the RPC host and return JSON:\n${request}`
      );

      if (!value) {
        return {
          content: [{ type: "text", text: "Emacs action was cancelled." }],
          details: { cancelled: true }
        };
      }

      return {
        content: [{ type: "text", text: value }],
        details: { action: params.action }
      };
    }
  });
}
