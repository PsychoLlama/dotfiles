{
  exports.editor =
    { host, ... }:

    {
      # An editor built without a host -- `packages.editor`, shared with people
      # who are not me -- has no owner, so it trusts nothing. `~` is expanded at
      # runtime by the env framework.
      config.env.trusted = host.trusted-directories or [ ];
    };
}
